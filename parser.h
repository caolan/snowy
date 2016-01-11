//#include <stdio.h>

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "http-parser/http_parser.h"

#define PARSER_DATA_VECTOR_SIZE 64

#define MESSAGE_BEGIN 1
#define URL 2
#define HEADER_FIELD 3
#define HEADER_VALUE 4
#define HEADERS_COMPLETE 5
#define BODY 6
#define MESSAGE_COMPLETE 7

typedef struct {
    uint16_t* vector;
    int vector_index;
    char* input_pointer;
    int input_index;
} http_parser_data;

/* Executes the parser. Returns number of parsed bytes. Sets
 * `parser->http_errno` on error. */
size_t custom_parser_execute(http_parser* parser,
                             const http_parser_settings* settings,
                             const char* str,
                             size_t len);

void free_parser(http_parser* parser);
http_parser_settings* make_settings(void);
http_parser* make_parser(uint16_t* vector);

/**
 * Executed at begin of message.
 */
int http_message_begin_cb(http_parser* parser);

/**
 * Executed when parsed the url.
 */
int http_url_cb(http_parser* parser, const char* chunk, size_t len);

/**
 * Executed on each header field.
 */
int http_header_field_cb(http_parser* parser, const char* chunk, size_t len);

/**
 * Executed on each header value.
 */
int http_header_value_cb(http_parser* parser, const char* chunk, size_t len);

/**
 * Executed when completed header parsing.
 */
int http_headers_complete_cb(http_parser* parser);

/**
 * Executed on body
 */
int http_body_cb(http_parser* parser, const char* chunk, size_t len);

/**
 * Is executed when request fully parsed.
 * User can read all request options from "&parser->data.request".
 */
int http_message_complete_cb(http_parser* parser);
