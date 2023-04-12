sum_d = function(...)
{
	cat("The values you passed: ",...,"\n");
	x = 0;
	y = unlist(list(...));
	for(i in 1:length(y))
	{
		x = x + y[i];
	}
	cat("The sum of the values: ",x,"\n");
}