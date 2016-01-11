#include "chicken.h"
#include "parser.h"

// TODO: do we need the vector_index check here if we're using
// CHECK_REMAINING_DATA_VECTOR everywhere required?
#define ADD_PART(parser_data, n)                                    \
    if ((parser_data)->vector_index >= PARSER_DATA_VECTOR_SIZE) return 1;       \
    (parser_data)->vector[parser_data->vector_index++] = (n);

#define FF_INPUT(parser_data, chunk)                                     \
    while ((parser_data)->input_pointer != (chunk)) {                    \
        (parser_data)->input_pointer = (parser_data)->input_pointer + 1; \
        (parser_data)->input_index++;                                    \
    }

#define ADD_SLICE(parser_data, cb_type, chunk, len)             \
    FF_INPUT(parser_data, chunk);                               \
    ADD_PART(parser_data, cb_type);                             \
    ADD_PART(parser_data, parser_data->input_index);            \
    ADD_PART(parser_data, parser_data->input_index + len);

// NOTE: callbacks currently only add 3 items MAX to vector, so we
// check if there are at least 3 slots left in the vector.
// Pauses the parser if we're running out of space to record
// callback events
#define CHECK_REMAINING_DATA_VECTOR(parser, parser_data) \
    if ((parser_data)->vector_index >= PARSER_DATA_VECTOR_SIZE - 3) { \
        http_parser_pause((parser), 1); \
    }

size_t custom_parser_execute(http_parser* parser,
                             const http_parser_settings* settings,
                             const char* str,
                             size_t len) {
    http_parser_data* data = parser->data;
    memset(data->vector, 0, PARSER_DATA_VECTOR_SIZE * sizeof(uint16_t));
    data->input_pointer = (char*)str;
    data->vector_index = 0;
    data->input_index = 0;
    return http_parser_execute(parser, settings, str, len);
}

void free_parser(http_parser* parser) {
    free(parser->data);
    free(parser);
}

/**
 * Initialize settings and assign parser callback handlers
 */
http_parser_settings* make_settings(void) {
    http_parser_settings* settings = malloc(sizeof(http_parser_settings));
    settings->on_url = http_url_cb;
    settings->on_body = http_body_cb;
    settings->on_header_field = http_header_field_cb;
    settings->on_header_value = http_header_value_cb;
    settings->on_headers_complete = http_headers_complete_cb;
    settings->on_message_begin = http_message_begin_cb;
    settings->on_message_complete = http_message_complete_cb;
    return settings;
}

/**
 * Initialize a new request parser
 */
http_parser* make_parser(uint16_t* vector) {
    http_parser* parser = malloc(sizeof(http_parser));
    http_parser_init(parser, HTTP_REQUEST);
    http_parser_data* parser_data = malloc(sizeof(http_parser_data));
    parser_data->vector = vector;
    parser->data = parser_data;
    return parser;
}

/**
 * Initializes default values, counters.
 */
int http_message_begin_cb(http_parser* parser) {
    http_parser_data* data = parser->data;
    ADD_PART(data, MESSAGE_BEGIN);
    CHECK_REMAINING_DATA_VECTOR(parser, data);
    return 0;
}

/**
 * Copies url string to request->url.
 */
int http_url_cb(http_parser* parser, const char* chunk, size_t len) {
    http_parser_data* data = parser->data;
    ADD_SLICE(data, URL, chunk, len);
    CHECK_REMAINING_DATA_VECTOR(parser, data);
    return 0;
}

/**
 * Copy the header field name to the current header item.
 */
int http_header_field_cb(http_parser* parser, const char* chunk, size_t len) {
    http_parser_data* data = parser->data;
    ADD_SLICE(data, HEADER_FIELD, chunk, len);
    CHECK_REMAINING_DATA_VECTOR(parser, data);
    return 0;
}

/**
 * Now copy its assigned value.
 */
int http_header_value_cb(http_parser* parser, const char* chunk, size_t len) {
    http_parser_data* data = parser->data;
    ADD_SLICE(data, HEADER_VALUE, chunk, len);
    CHECK_REMAINING_DATA_VECTOR(parser, data);
    return 0;
}

/**
 * Extract the method name.
 */
int http_headers_complete_cb(http_parser* parser) {
    http_parser_data* data = parser->data;
    ADD_PART(data, HEADERS_COMPLETE);
    ADD_PART(data, parser->method);
    ADD_PART(data, http_should_keep_alive(parser));
    CHECK_REMAINING_DATA_VECTOR(parser, data);
    return 0;
}

/**
 * And copy the body content.
 */
int http_body_cb(http_parser* parser, const char* chunk, size_t len) {
    http_parser_data* data = parser->data;
    ADD_SLICE(data, BODY, chunk, len);
    CHECK_REMAINING_DATA_VECTOR(parser, data);
    return 0;
}

/**
 * Last cb executed by http_parser.
 */
int http_message_complete_cb(http_parser* parser) {
    http_parser_data* data = parser->data;
    ADD_PART(data, MESSAGE_COMPLETE);
    http_parser_pause(parser, 1);
    return 0;
}
