fuggetlenseg = function(F)
{
	Frow = rowSums(F);
	Fcol = colSums(F);
	n = sum(Frow);
	cat('Az n értéke: ',n,'\n');

	sum = 0;

	for(i in 1:nrow(F))
	{
		for(j in 1:ncol(F))
		{
			sum = sum + ((F[i,j] - ((Frow[i]*Fcol[j]))/n)^2)/(Frow[i]*Fcol[j]);
		}
	}
	sum = sum * n;
	cat('A kiszámított érték: ',sum,'\n');

	df = (nrow(F) - 1) * (ncol(F) - 1);
	cat('A szabadsági fok: ',df,'\n');
	chi = qchisq(.95, df);
	cat('Viszonyított érték (95%-os konfidenciaintervallum esetén): ',chi,'\n');
	
	if(sum < chi)
	{
		cat('Elfogadjuk a hipotézist.\n');
	} else
	{
		cat('Nem fogadjuk el a hipotézist.\n');
	}
}