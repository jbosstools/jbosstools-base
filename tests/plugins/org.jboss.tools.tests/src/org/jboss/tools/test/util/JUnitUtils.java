/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 

package org.jboss.tools.test.util;

import java.io.PrintWriter;
import java.io.StringWriter;

import junit.framework.TestCase;

/**
 * @author eskimo
 *
 */
public class JUnitUtils {
	
	public static void fail(String message,Exception e) {
		StringWriter out = new StringWriter();
		out.append(message).append('\n');
		e.printStackTrace(new PrintWriter(out));
		TestCase.fail(out.getBuffer().toString());
	}
}
