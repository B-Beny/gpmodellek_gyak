mean_d = function(...)
{
	cat("The values you passed: ",...,"\n");
	x = 0;
	y = unlist(list(...));
	for(i in 1:length(y))
	{
		x = x + y[i];
	}
	x = x/length(y);
	cat("The mean of the values: ",x,"\n");
}