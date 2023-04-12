s_n_star_2 = function(a)
{
	Mean = mean(a);
	cat("Átlag: ", Mean, "\n");
	frac = 1/(length(a)-1);
	Sum = 0;
	for(i in 1:length(a))
	{
		Sum = Sum + (a[i] - Mean)^2;
	}
	cat("Korrigált empírikus szórásnégyzet: ", frac * Sum,"\n");
}
