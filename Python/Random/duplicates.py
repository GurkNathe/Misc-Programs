
def cont_dup(nums):
    seen = {}
    for num in nums:
        if num in seen.keys():
            return True
        seen[num] = 1
    return False

print(cont_dup([1,1,1,2,2,3,4]))