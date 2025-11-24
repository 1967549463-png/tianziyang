## makeCacheMatrix:
## 该函数创建一个特殊的“矩阵”对象，可以缓存其逆。
## 它实际上返回一个包含四个函数的列表：
## 1. set(y): 设置矩阵的值。
## 2. get(): 获取矩阵的值。
## 3. setInverse(inverse): 设置（缓存）矩阵的逆。
## 4. getInverse(): 获取缓存的逆。

makeCacheMatrix <- function(x = matrix()) {
    # 'i' 用于存储矩阵的逆，初始设置为 NULL
    i <- NULL
    
    # 设置新矩阵 'y' 的函数
    set <- function(y) {
        x <<- y # 将输入矩阵 'y' 存储到父环境的 'x' 中
        i <<- NULL # 矩阵一旦改变，就清除缓存的逆 'i'
    }
    
    # 获取矩阵 'x' 的函数
    get <- function() x
    
    # 设置矩阵的逆 'inverse' 的函数（缓存）
    setInverse <- function(inverse) i <<- inverse
    
    # 获取缓存的逆 'i' 的函数
    getInverse <- function() i
    
    # 返回一个包含上述四个函数的列表
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
## cacheSolve:
## 该函数计算 makeCacheMatrix 返回的特殊“矩阵”的逆。
## 如果逆已经计算过（且矩阵未发生变化），则 cachesolve 应从缓存中获取逆。

cacheSolve <- function(x, ...) {
    # 尝试从 'x' 对象中获取缓存的逆
    i <- x$getInverse()
    
    # 检查逆是否已缓存（即 'i' 不为 NULL）
    if(!is.null(i)) {
        message("获取缓存的矩阵逆...")
        return(i) # 直接返回缓存的逆
    }
    
    # 如果没有缓存，则获取实际的矩阵
    data <- x$get()
    
    # 计算矩阵的逆（使用 R 的 solve() 函数）
    i <- solve(data, ...)
    
    # 将计算出的逆存入 'x' 对象的缓存中
    x$setInverse(i)
    
    # 返回矩阵的逆
    return(i)
}
git clone https://github.com/tianziyang/ProgrammingAssignment2.git
