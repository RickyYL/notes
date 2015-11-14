# Introduction to Flex and Bison

## First Program

Flex 文件的基本格式：

```
%{
	// 声明代码部分
%}

%%
	// Token 匹配规则
%%

int main(int argc, char *argv[])
{
	// 程序主体
}
```

