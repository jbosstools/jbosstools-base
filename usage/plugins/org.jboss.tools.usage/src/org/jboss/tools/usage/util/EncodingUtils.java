/*******************************************************************************
 * Copyright (c) 2008 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.util;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

public class EncodingUtils {

	private static final String ENCODING_UTF8 = "UTF-8";

	/**
	 * Encodes the given string in utf8 while catching exceptions that may
	 * occur. If an encoding exception occurs, <tt>null</tt> is returned
	 * 
	 * @param aString
	 *            the a string to be encoded
	 * @return the encoded string or <tt>null</tt> if an error occured while
	 *         encoding
	 */
	public static String checkedEncodeUtf8(String aString) {
		try {
			return URLEncoder.encode(aString, ENCODING_UTF8);
		} catch (UnsupportedEncodingException e) {
			return aString;
		}
	}

}
