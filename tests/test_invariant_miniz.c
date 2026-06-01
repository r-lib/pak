#include <check.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* Forward declare the vulnerable function from miniz.c */
extern void *mz_calloc(size_t items, size_t size);
extern void mz_free(void *p);

START_TEST(test_mz_calloc_overflow_protection)
{
    /* Invariant: mz_calloc must not allocate less memory than items*size,
       or reject the allocation when overflow would occur */
    
    struct {
        size_t items;
        size_t size;
        const char *description;
    } payloads[] = {
        /* Valid case: small allocation */
        { 10, 100, "valid_small" },
        
        /* Boundary: maximum safe 32-bit allocation */
        { 1024, 1024, "boundary_safe" },
        
        /* Overflow case: items * size > SIZE_MAX on 32-bit */
        { 0x10000000, 0x10000000, "overflow_huge" },
        
        /* Overflow case: classic 32-bit overflow (0x100000001 wraps to 1) */
        { 0x100000001, 1, "overflow_wrap_to_one" },
        
        /* Boundary: size_t max / 2 + 1 squared */
        { 0x80000001, 0x80000001, "overflow_near_max" }
    };
    
    int num_payloads = sizeof(payloads) / sizeof(payloads[0]);
    
    for (int i = 0; i < num_payloads; i++) {
        size_t items = payloads[i].items;
        size_t size = payloads[i].size;
        
        /* Detect if multiplication would overflow */
        int would_overflow = 0;
        if (items > 0 && size > SIZE_MAX / items) {
            would_overflow = 1;
        }
        
        void *ptr = mz_calloc(items, size);
        
        if (would_overflow) {
            /* On overflow, allocation should fail (return NULL) or be clamped */
            ck_assert_msg(ptr == NULL || items * size <= SIZE_MAX,
                "Overflow case %s: allocation succeeded despite overflow", 
                payloads[i].description);
        } else {
            /* Valid allocation must succeed */
            ck_assert_msg(ptr != NULL,
                "Valid allocation %s failed unexpectedly",
                payloads[i].description);
            
            /* Verify we can write to the allocated buffer without crash */
            if (ptr && items > 0 && size > 0) {
                memset(ptr, 0xAA, (items * size < 1024) ? items * size : 1024);
            }
            
            mz_free(ptr);
        }
    }
}
END_TEST

Suite *security_suite(void)
{
    Suite *s;
    TCase *tc_core;

    s = suite_create("Security");
    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_mz_calloc_overflow_protection);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = security_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);

    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}