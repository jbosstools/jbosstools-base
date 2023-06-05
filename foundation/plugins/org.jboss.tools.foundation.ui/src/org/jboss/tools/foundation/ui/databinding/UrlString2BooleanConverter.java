/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.ui.databinding;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.databinding.conversion.Converter;

/**
 * Converts an url in string form to a boolean. Returns true if the string is a
 * valid url. The validity check is a simplified check for a valid url.
 * 
 * @author André Dietisheim
 * 
 */
public class UrlString2BooleanConverter extends Converter {


	public UrlString2BooleanConverter() {
		super(String.class, Boolean.class);
	}

	public Object convert(Object fromObject) {
		if (!(fromObject instanceof String)) {
			return Boolean.FALSE;
		}

		return toUrl((String) fromObject) != null;
	}

	private URL toUrl(String url) {
		try {
			if (!new SimpleUrlStringChecker(url).isValid()) {
				return null;
			}
			return new URL(url);
		} catch (MalformedURLException e) {
			return null;
		}
	}
}