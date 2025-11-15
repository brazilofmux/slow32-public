// mux_vsnprintf - Is an sprintf-like function that will not overflow
// a buffer of specific size. The size is give by count, and count
// should be chosen to include the '\0' termination.
//
// Returns: A number from 0 to count-1 that is the string length of
// the returned (possibly truncated) buffer.
//
size_t DCL_CDECL mux_vsnprintf(UTF8 *pBuffer, size_t nBuffer, const UTF8 *pFmt, va_list va)
{
    if (  nullptr == pBuffer
       || nBuffer < 1)
    {
       return 0;
    }
    size_t nLimit = nBuffer-1;

    // Rather than copy a character at a time, some copies are deferred and performed in a single request.
    //
    size_t iFmtDeferred = 0;
    size_t dDeferred = 0;

    size_t iBuffer = 0;
    size_t ncpFmt;
    size_t iFmt = 0;
    if (  nullptr != pFmt
       && utf8_strlen(pFmt, ncpFmt))
    {
        static UTF8 Buff[I64BUF_SIZE];

        while (0 != ncpFmt)
        {
            if ('%' != pFmt[iFmt])
            {
                // Ordinary character.
                //
                size_t d = utf8_FirstByte[pFmt[iFmt]];
                size_t dProposed = dDeferred + d;
                if (nLimit < iBuffer + dProposed)
                {
                    if (0 < dDeferred)
                    {
                        // Unravel the deferred copy.
                        //
                        memcpy(pBuffer + iBuffer, pFmt + iFmtDeferred, dDeferred);
                        iBuffer += dDeferred;
                        dDeferred = 0;
                    }
                    goto done;
                }
                else if (0 == dDeferred)
                {
                    iFmtDeferred = iFmt;
                }
                dDeferred = dProposed;

                iFmt += d;
                ncpFmt--;
            }
            else
            {
                if (0 < dDeferred)
                {
                    // Unravel the deferred copy.
                    //
                    memcpy(pBuffer + iBuffer, pFmt + iFmtDeferred, dDeferred);
                    iBuffer += dDeferred;
                    dDeferred = 0;
                }

                size_t cbBuff;
                size_t cpBuff;
                size_t nWidth = 0;
                size_t nPrecision = 0;
                bool bLeft = false;
                bool bZeroPadded = false;
                bool bWidth = false;
                bool bSawPeriod = false;
                bool bPrecision = false;
                int nLongs = 0;

                iFmt++;
                ncpFmt--;

                while (0 != ncpFmt)
                {
                    if (  'd' == pFmt[iFmt]
                       || 's' == pFmt[iFmt]
                       || 'u' == pFmt[iFmt]
                       || 'x' == pFmt[iFmt]
                       || 'X' == pFmt[iFmt]
                       || 'p' == pFmt[iFmt])
                    {
                        UTF8 *pBuff = Buff;

                        if ('d' == pFmt[iFmt])
                        {
                            // Obtain and validate argument.
                            //
                            if (0 == nLongs)
                            {
                                int i = va_arg(va, int);
                                cbBuff = cpBuff = mux_ltoa(i, Buff);
                            }
                            else if (1 == nLongs)
                            {
                                long int i = va_arg(va, long int);
                                cbBuff = cpBuff = mux_ltoa(i, Buff);
                            }
                            else if (2 == nLongs)
                            {
                                INT64 i = va_arg(va, INT64);
                                cbBuff = cpBuff = mux_i64toa(i, Buff);
                            }
                            else
                            {
                                goto done;
                            }
                        }
                        else  if ('s' == pFmt[iFmt])
                        {
                            // Obtain and validate argument.
                            //
                            pBuff = va_arg(va, UTF8 *);
                            if (  !utf8_strlen(pBuff, cpBuff)
                               || 0 != nLongs)
                            {
                                goto done;
                            }
                            cbBuff = strlen((char *)pBuff);

                            if (  bPrecision
                               && nPrecision < cpBuff)
                            {
                                // Need to walk cbBuff back to correspond to changes in cpBuff.
                                //
                                while (cpBuff != nPrecision)
                                {
                                    do
                                    {
                                        cbBuff--;
                                    } while (UTF8_CONTINUE <= utf8_FirstByte[pBuff[cbBuff]]);
                                    cpBuff--;
                                }
                            }
                        }
                        else if ('p' == pFmt[iFmt])
                        {
                            if (  0 != nLongs
                               || bWidth)
                            {
                                goto done;
                            }

                            // Convert pointer to unsigned integer.
                            //
                            union
                            {
                                MUX_UINT_PTR ui;
                                void *pv;
                            } u;
                            u.pv = va_arg(va, void *);
#if SIZEOF_UINT_PTR <= SIZEOF_UNSIGNED_LONG
                            cbBuff = cpBuff = mux_utox(u.ui, Buff, true);
#elif SIZEOF_UINT_PTR <= SIZEOF_UNSIGNED_LONG_LONG
                            cbBuff = cpBuff = mux_ui64tox(u.ui, Buff, true);
#else
#error Size of pointer is larger size of largest known integer.
#endif
                            bWidth = true;
                            nWidth = 2*sizeof(MUX_UINT_PTR);
                            bZeroPadded = true;
                        }
                        else
                        {
                            bool bHex = (  'x' == pFmt[iFmt]
                                        || 'X' == pFmt[iFmt]);
                            bool bUpper = ('X' == pFmt[iFmt]);

                            // Obtain and validate argument.
                            //
                            if (0 == nLongs)
                            {
                                unsigned int ui = va_arg(va, unsigned int);
                                cbBuff = cpBuff = bHex?mux_utox(ui, Buff, bUpper):mux_utoa(ui, Buff);
                            }
                            else if (1 == nLongs)
                            {
                                unsigned long int ui = va_arg(va, unsigned long int);
                                cbBuff = cpBuff = bHex?mux_utox(ui, Buff, bUpper):mux_utoa(ui, Buff);
                            }
                            else if (2 == nLongs)
                            {
                                UINT64 ui = va_arg(va, UINT64);
                                cbBuff = cpBuff = bHex?mux_ui64tox(ui, Buff, bUpper):mux_ui64toa(ui, Buff);
                            }
                            else
                            {
                                goto done;
                            }
                        }

                        // Calculate and validate needed size.  Numberic and
                        // string fields are at least the size of their width.
                        // String fields may have been truncated above by
                        // precision.
                        //
                        // Width is compared with the number of code points.
                        // Padding is always done with space or zero.
                        //
                        size_t nUsed = cbBuff;
                        size_t nPadding = 0;
                        if (  bWidth
                           && cpBuff < nWidth)
                        {
                            nPadding = nWidth - cpBuff;
                            nUsed += nPadding;
                        }

                        if (nLimit < iBuffer + nUsed)
                        {
                            goto done;
                        }

                        // Apply leading padding if necessary.
                        //
                        if (  !bLeft
                           && bWidth)
                        {
                            if (  'd' == pFmt[iFmt]
                               && '-' == pBuff[0]
                               && 0 < nPadding
                               && bZeroPadded)
                            {
                                // The leading minus sign must be laid down before zero-padding begins.
                                //
                                pBuffer[iBuffer] = '-';
                                iBuffer++;

                                pBuff++;
                                cbBuff--;
                                cpBuff--;
                            }

                            while (0 < nPadding)
                            {
                                pBuffer[iBuffer] = bZeroPadded?'0':' ';
                                iBuffer++;
                                nPadding--;
                            }
                        }

                        // Apply string.
                        //
                        memcpy(pBuffer + iBuffer, pBuff, cbBuff);
                        iBuffer += cbBuff;

                        // Apply trailing padding if necessary.
                        //
                        if (  bLeft
                           && bWidth)
                        {
                            while (0 < nPadding)
                            {
                                pBuffer[iBuffer] = bZeroPadded?'0':' ';
                                iBuffer++;
                                nPadding--;
                            }
                        }

                        iFmt++;
                        ncpFmt--;
                        break;
                    }
                    else if ('l' == pFmt[iFmt])
                    {
                        nLongs++;
                        iFmt++;
                        ncpFmt--;
                    }
                    else if (  '0' <= pFmt[iFmt]
                            && pFmt[iFmt] <= '9')
                    {
                        if (!bSawPeriod)
                        {
                            if (!bWidth)
                            {
                                if ('0' == pFmt[iFmt])
                                {
                                    if (bZeroPadded)
                                    {
                                        goto done;
                                    }
                                    bZeroPadded = true;
                                }
                                else
                                {
                                    nWidth = pFmt[iFmt] - '0';
                                    bWidth = true;
                                }
                            }
                            else
                            {
                                nWidth = 10 * nWidth + pFmt[iFmt] - '0';
                            }
                        }
                        else
                        {
                            if (!bPrecision)
                            {
                                nPrecision = pFmt[iFmt] - '0';
                                bPrecision = true;
                            }
                            else
                            {
                                nPrecision = 10 * nPrecision + pFmt[iFmt] - '0';
                            }
                        }

                        iFmt++;
                        ncpFmt--;
                    }
                    else if ('.' == pFmt[iFmt])
                    {
                        bSawPeriod = true;

                        iFmt++;
                        ncpFmt--;
                    }
                    else if ('-' == pFmt[iFmt])
                    {
                        if (bLeft)
                        {
                            goto done;
                        }
                        bLeft = true;

                        iFmt++;
                        ncpFmt--;
                    }
                    else if ('c' == pFmt[iFmt])
                    {
                        unsigned int ch = va_arg(va, unsigned int);
                        if (nLimit < iBuffer + 1)
                        {
                            goto done;
                        }
                        pBuffer[iBuffer] = static_cast<UTF8>(ch);
                        iBuffer++;

                        iFmt++;
                        ncpFmt--;
                        break;
                    }
                    else if ('%' == pFmt[iFmt])
                    {
                        // "%%"
                        //
                        if (nLimit < iBuffer + 1)
                        {
                            goto done;
                        }
                        pBuffer[iBuffer] = '%';
                        iBuffer++;

                        iFmt++;
                        ncpFmt--;
                        break;
                    }
                    else
                    {
                        mux_assert(0);

                        iFmt += utf8_FirstByte[pFmt[iFmt]];
                        ncpFmt--;
                    }
                }
            }
        }

        if (0 < dDeferred)
        {
            // Unravel the deferred copy.
            //
            memcpy(pBuffer + iBuffer, pFmt + iFmtDeferred, dDeferred);
            iBuffer += dDeferred;
            dDeferred = 0;
        }
    }

done:
    pBuffer[iBuffer] = '\0';
    return iBuffer;
}

void DCL_CDECL mux_sprintf(UTF8 *buff, size_t count, const UTF8 *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    (void)mux_vsnprintf(buff, count, fmt, ap);
    va_end(ap);
}
