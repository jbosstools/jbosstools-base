/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.googleanalytics;

import org.eclipse.core.runtime.Assert;

public class UrlCookie {

	private CharSequence value;
	private String identifier;
	private char delimiter;

	public UrlCookie(String identifier, CharSequence value, char delimiter) {
		Assert.isTrue(identifier != null && identifier.length() > 0);
		Assert.isTrue(value != null && value.length() > 0);

		this.identifier = identifier;
		this.value = value;
		this.delimiter = delimiter;
	}

	public UrlCookie(String identifier, CharSequence value) {
		this(identifier, value, (char) -1);
	}

	public void appendTo(StringBuilder builder) {
		builder.append(identifier)
				.append(IGoogleAnalyticsParameters.EQUALS_SIGN)
				.append(value);
		if (delimiter != (char) -1) {
			builder.append(delimiter);
		}
	}
}
