sum = function(a)
{
	cat("The values you passed: ",a,"\n");
	x = 0;
	for(i in 1:length(a))
	{
		x = x + a[i];
	}
	cat("The sum of the values: ",x,"\n");
}