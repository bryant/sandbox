import numpy

t = numpy.linspace(0, 256, 120)
x = (0.0002*t*t + numpy.sin(t)).reshape(-1, 1)

def K(T):
    """
    returns a (T-2, T)-shaped matrix corresponding to:
        k_ij | 1 if i == j or i == j+2
             | -2 if i == j+1
             | 0 otherwise
    """
    row = numpy.hstack((numpy.array([1, -2, 1]), numpy.zeros(max(T-3, 0))))
    return numpy.vstack(numpy.roll(row, i) for i in xrange(max(T-2, 1)))

def hodrick_prescott(samples, smoothness=1600):
    """
    returns the trending component of the hodrick-prescott filter.
    @samples: rows, observations
    """
    T = samples.shape[0]
    k = K(T)
    return numpy.linalg.inv(smoothness*numpy.transpose(k).dot(k) +
                            numpy.eye(T)).dot(samples)
