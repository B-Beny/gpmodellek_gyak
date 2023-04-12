emp = function(X)
{
	s = 0;
	for(i in 1:length(X))
	{
		s = s + X[i];
	}
	H = s/length(X);
	return(H);
}

# ez egy komment
# itt folytatódik

u_prob <- function(x, m, s)
{
	n = length(x);
	u = (emp(x)-m)/(s/sqrt(n));
	
	if(abs(u) < 1.96)
	{
		cat("ELFOGADJUK \n");
	} else
	{
		cat("NEM FOGADJUK EL \n");
	}
}