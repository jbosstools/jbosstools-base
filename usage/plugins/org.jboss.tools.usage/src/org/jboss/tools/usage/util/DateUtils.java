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
package org.jboss.tools.usage.util;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;


public class DateUtils {

	private DateUtils() {
	}

	private static final DateFormat DATE_FORMAT = DateFormat.getDateTimeInstance();

	public static Date checkedParseDateString(String date) {
		try {
			return DATE_FORMAT.parse(date);
		} catch (ParseException e) {
			return null;
		}
	}

}
