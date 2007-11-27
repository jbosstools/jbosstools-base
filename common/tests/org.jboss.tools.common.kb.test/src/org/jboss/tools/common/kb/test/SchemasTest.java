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
package org.jboss.tools.common.kb.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class SchemasTest extends TestCase {

	public static Test suite() {
		return new TestSuite(SchemasTest.class);
	}

	public void testSchemas() {
		/* TODO change KbTldStore and uncomment following code
		String errorMessage = null;
		HashMap resources = null;
		try {
			resources = KbTldStore.getInstance().getResourcesFromExtentionPoits(false);
		} catch (KbException e) {
			errorMessage = e.getMessage();
		}
		Set keys = resources.keySet();
		for (Object key : keys) {
			KbTldResource kbResource = (KbTldResource)resources.get(key);
			File schemaLocation = kbResource.getSchemaLocation();
			try {
				KbDocumentBuilderFactory.createDocumentBuilder(false).parse(schemaLocation);
			} catch (Exception e) {
				errorMessage = "ERROR: Can't parse Schema (location: " + schemaLocation + "). " + e.getMessage();
				break;
			}
		}
		assertNull(errorMessage, errorMessage);
		*/
	}
}