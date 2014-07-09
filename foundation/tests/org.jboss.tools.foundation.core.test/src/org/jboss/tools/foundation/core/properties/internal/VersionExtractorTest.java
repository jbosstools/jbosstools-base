/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.core.properties.internal;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class VersionExtractorTest {

	private String packageName = getClass().getPackage().getName();

	@Test
	public void testExtraWhitespace() {
		String version = VersionExtractor.getVersion(packageName + ".whitespaceversion", getClass().getClassLoader());
		assertEquals("X.Y.Z", version);
	}
	

	@Test
	public void testFilteredVersion() {
		String version = VersionExtractor.getVersion(packageName + ".filteredversion", getClass().getClassLoader());
		assertEquals("Z.Y.X", version);
	}

}
