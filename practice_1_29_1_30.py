#递归计算过程
def sum(term,a,next,b):
    if a > b:
        return 0
    else:
        return term(a) + sum(next(a),b,term,next)

#迭代计算过程
def sum(term,a,next,b):
    total, k = 0, a
    while k <= b:
        total, k = total + term(k), next(k)
    return total
    
#求函数f在a到b上的积分
def integral(f,a,b,dx):
    def add_dx(x):
        return dx + x
    return dx * sum(f,a + dx/2,add_dx,b)
    
#辛普森方法
def Simpson_integral(f,a,b,n):
    def term(x):
        return f(a + x * h)
    def high_term(y):
        if even(y):
            return term(y) * 2
        else:
            return term(y) * 4
    def next(z):
        return z + 1
    h = (a + b) / n
    return h/3 * (term(0) + term(n) + sum(high_term , 1, next , n-1))
    
    
 '''
from cmath import sin,pi

print(integral(sin , 0 , pi , .001))
print(Simpson_integral(sin,0,pi,10000))


#结果

(2.000000000367955+0j)
(1.9999999999999925+0j)

 '''
