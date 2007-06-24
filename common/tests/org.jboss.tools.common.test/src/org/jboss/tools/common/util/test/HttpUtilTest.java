/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.util.test;

import org.apache.commons.httpclient.HttpClient;
import org.jboss.tools.common.util.HttpUtil;

import junit.framework.TestCase;

public class HttpUtilTest extends TestCase {

	public void testCreateHttpClient() {
		try {
			HttpClient client  = HttpUtil.createHttpClient("http://www.jboss.com");
			assertNotNull("HTTP Client object wasn't created", client);
		} catch (Exception e) {
			fail("Error occurs when creating HTTP Client object");
		}
	}
}
