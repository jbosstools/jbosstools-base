/*******************************************************************************
 * Copyright (c) 2016-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.reddeer.utils;

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * Class providing utils for Stack Trace.
 * 
 * @author rhopp, jkopriva
 *
 */

public class StackTraceUtils {

	/**
	 * Converts exception Stack Trace to String.
	 * 
	 * @param ex
	 *            Exception
	 * @return String with Stack Trace message
	 */
	public static String stackTraceToString(Exception ex) {
		StringWriter sw = new StringWriter();
		ex.printStackTrace(new PrintWriter(sw));
		return sw.toString();
	}

}
