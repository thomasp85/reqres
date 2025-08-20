# fiery

<details>

* Version: 1.2.1
* GitHub: https://github.com/thomasp85/fiery
* Source code: https://github.com/cran/fiery
* Date/Publication: 2024-02-05 22:40:11 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "fiery")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      
      `actual$Content-Type` is absent
      `expected$Content-Type` is a character vector ('text/plain')
      
      `actual$X-Powered-By` is absent
      `expected$X-Powered-By` is a character vector ('fiery')
      
      [ FAIL 2 | WARN 0 | SKIP 4 | PASS 246 ]
      Error: Test failures
      Execution halted
    ```

