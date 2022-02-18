amscale_constructor <- function(stimuli, respondent, fit, ninput, nresp, nstim) {

  stopifnot(is.numeric(stimuli), is.atomic(stimuli))
  stopifnot(is.data.frame(respondent))
  stopifnot(is.numeric(fit), is.atomic(fit))
  stopifnot(is.numeric(ninput), is.atomic(ninput))
  stopifnot(is.numeric(nresp), is.atomic(nresp))
  stopifnot(is.numeric(nstim), is.atomic(nstim))

  out <- structure(list(stimuli = stimuli,
                        respondents = respondent,
                        fit = fit,
                        ninput = ninput,
                        nresp = nresp,
                        nstim = nstim),
                   class="amscale")

  return(out)

}
