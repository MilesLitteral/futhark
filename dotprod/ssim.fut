module ssim () = {
    def check_shape_equality(im1: []i64, im2: []i64, window_size: f32, gaussian_weights: bool):
        --Raise an error if the shape do not match

        if im1.shape != im2.shape:
            print('Input images must have the same dimensions.')
        return

    def structural_similarity(im1: []f32, im2: []f32, args:[]i64):
        check_shape_equality(im1, im2)
        float_type = _supported_float_type(im1.dtype)

        if channel_axis is not None:
            -- loop over channels
            args = dict(win_size=win_size,
                        gradient=gradient,
                        data_range=data_range,
                        channel_axis=None,
                        gaussian_weights=gaussian_weights,
                        full=full)
            args.update(kwargs)
            nch = im1.shape[channel_axis]
            mssim = np.empty(nch, dtype=float_type)

            if gradient:
                G = np.empty(im1.shape, dtype=float_type)
            if full:
                S = np.empty(im1.shape, dtype=float_type)
            channel_axis = channel_axis % im1.ndim
            _at = functools.partial(utils.slice_at_axis, axis=channel_axis)
            map (\i -> structural_similarity(im1[i], im2[i]) (args)
            for ch in range(nch):
                ch_result = structural_similarity(im1[_at(ch)],
                                                im2[_at(ch)], **args)
                if gradient and full:
                    mssim[ch], G[_at(ch)], S[_at(ch)] = ch_result
                elif gradient:
                    mssim[ch], G[_at(ch)] = ch_result
                elif full:
                    mssim[ch], S[_at(ch)] = ch_result
                else:
                    mssim[ch] = ch_result
            mssim = mssim.mean()
            if gradient and full:
                return mssim, G, S
            elif gradient:
                return mssim, G
            elif full:
                return mssim, S
            else:
                return mssim

        K1 = kwargs.pop('K1', 0.01)
        K2 = kwargs.pop('K2', 0.03)
        sigma = kwargs.pop('sigma', 1.5)
        if K1 < 0:
            raise ValueError("K1 must be positive")
        if K2 < 0:
            raise ValueError("K2 must be positive")
        if sigma < 0:
            raise ValueError("sigma must be positive")
        use_sample_covariance = kwargs.pop('use_sample_covariance', True)

        if gaussian_weights == true:
            # Set to give an 11-tap filter with the default sigma of 1.5 to match
            # Wang et. al. 2004.
            truncate = 3.5

        if win_size > 0:
            if gaussian_weights:
                --set win_size used by crop to match the filter size
                r = int(truncate * sigma + 0.5)  # radius as in ndimage
                win_size = 2 * r + 1
            else:
                win_size = 7   # backwards compatibility

        if np.any((np.asarray(im1.shape) - win_size) < 0):
            raise ValueError(
                'win_size exceeds image extent. '
                'Either ensure that your images are '
                'at least 7x7; or pass win_size explicitly '
                'in the function call, with an odd value '
                'less than or equal to the smaller side of your '
                'images. If your images are multichannel '
                '(with color channels), set channel_axis to '
                'the axis number corresponding to the channels.')

        if not (win_size % 2 == 1):
            raise ValueError('Window size must be odd.')

        if data_range == None:
            if im1.dtype != im2.dtype:
                warn("Inputs have mismatched dtype.  Setting data_range based on "
                    "im1.dtype.", stacklevel=2)
            dmin, dmax = dtype_range[im1.dtype.type]
            data_range = dmax - dmin

        ndim = im1.ndim

        if gaussian_weights == true:
            filter_func = gaussian
            filter_args = {'sigma': sigma, 'truncate': truncate, 'mode': 'reflect'}
        else:
            filter_func = uniform_filter
            filter_args = {'size': win_size}

        --ndimage filters need floating point data
        im1 = im1.astype(float_type, copy=False)
        im2 = im2.astype(float_type, copy=False)

        NP = win_size ** ndim

        --filter has already normalized by NP
        if use_sample_covariance:
            cov_norm = NP / (NP - 1)  # sample covariance
        else:
            cov_norm = 1.0  # population covariance to match Wang et. al. 2004

        --compute (weighted) means
        ux = filter_func(im1, **filter_args)
        uy = filter_func(im2, **filter_args)

        --compute (weighted) variances and covariances
        uxx = filter_func(im1 * im1, **filter_args)
        uyy = filter_func(im2 * im2, **filter_args)
        uxy = filter_func(im1 * im2, **filter_args)
        vx = cov_norm * (uxx - ux * ux)
        vy = cov_norm * (uyy - uy * uy)
        vxy = cov_norm * (uxy - ux * uy)

        R = data_range
        C1 = (K1 * R) ** 2
        C2 = (K2 * R) ** 2

        A1, A2, B1, B2 = ((2 * ux * uy + C1,
                        2 * vxy + C2,
                        ux ** 2 + uy ** 2 + C1,
                        vx + vy + C2))
        D = B1 * B2
        S = (A1 * A2) / D

        --to avoid edge effects will ignore filter radius strip around edges
        pad = (win_size - 1) // 2

        --compute (weighted) mean of ssim. Use float64 for accuracy.
        mssim = crop(S, pad).mean(dtype=np.float64)

        if gradient:
            -- The following is Eqs. 7-8 of Avanaki 2009.
            grad = filter_func(A1 / D, **filter_args) * im1
            grad += filter_func(-S / B2, **filter_args) * im2
            grad += filter_func((ux * (A2 - A1) - uy * (B2 - B1) * S) / D,
                                **filter_args)
            grad *= (2 / im1.size)

            if full:
                return mssim, grad, S
            else:
                return mssim, grad
        else:
            if full:
                return mssim, S
            else:
                return mssim
}