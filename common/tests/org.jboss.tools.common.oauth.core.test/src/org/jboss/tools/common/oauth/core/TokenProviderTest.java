/*******************************************************************************
 * Copyright (c) 2021 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v2.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v20.html
 *
 * Contributors:
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.oauth.core;

import junit.framework.TestCase;

/**
 * @author Red Hat Developers
 *
 */
public class TokenProviderTest extends TestCase {

	public static void testVerifyTokenProvider() {
		assertNotNull(TokenProvider.get());
	}

}
