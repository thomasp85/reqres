context("request")

rook <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = '{"name":["Thomas Lin Pedersen"],"age":[31],"homepage":["www.data-imaginist.com","www.github.com/thomasp85"]}',
    headers = list(
        Content_Type = 'application/json',
        Accept = 'application/json, application/xml; q=0.5, text/*; q=0.3',
        Accept_Encoding = 'gzip, br',
        Cookie = 'id=Thomas; key=123',
        X_Forwarded_For = '500.0.0.0, 400.0.0.0',
        X_Forwarded_Host = 'www.example.com:80',
        X_Forwarded_Proto = 'https'
    ),
    REMOTE_ADDR = '230.45.12.45'
)

test_that('request gets created correctly', {
    req <- Request$new(rook)
    expect_null(req$body)
    expect_equal(req$host, "127.0.0.1:80")
    expect_false(req$trust)
    expect_equal(req$method, 'get')
    expect_equal(req$cookies, list(id = 'Thomas', key = '123'))
    expect_named(req$headers, c("Accept", "Accept_Encoding", "Content_Type",
                                "Cookie", "X_Forwarded_For", "X_Forwarded_Host",
                                "X_Forwarded_Proto"))
    expect_length(req$headers$Accept, 3)
    expect_equal(req$headers$Content_Type, 'application/json')
    expect_equal(req$ip, '230.45.12.45')
    expect_equal(req$ips, character(0))
    expect_equal(req$protocol, 'http')
    expect_null(req$root)
    expect_equal(req$path, '/summary')
    expect_equal(req$url, 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen')
    expect_equal(req$query, list(id = 2347, user = 'Thomas Lin Pedersen'))
    expect_false(req$xhr)
    expect_false(req$secure)
    expect_identical(req$rook, rook)
    expect_null(req$response)
    expect_output(print(req), 'A HTTP request')
})

test_that('trust works', {
    req <- Request$new(rook)
    expect_false(req$trust)
    expect_error(req$trust <- 'test')
    req$trust <- TRUE
    expect_true(req$trust)
    expect_equal(req$host, 'www.example.com:80')
    expect_equal(req$protocol, 'https')
    expect_true(req$secure)
    expect_equal(req$ip, '500.0.0.0')
    expect_equal(req$ips, c('500.0.0.0', '400.0.0.0'))
})
