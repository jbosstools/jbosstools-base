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
import static org.junit.Assert.assertNull;

import org.jboss.tools.foundation.core.properties.internal.SimpleHierarchicalVersion;
import org.junit.Test;

public class SimpleHierarchicalVersionTest {
	
	@Test
	public void testGetParentVersion() {
		assertEquals("1.2.3.alpha", new SimpleHierarchicalVersion("1.2.3.alpha.CR1.v20140211-1204-B52").getParentVersion().toString());
		assertEquals("1.2.3.CR1", new SimpleHierarchicalVersion("1.2.3.CR1.v20140211-1204-B52").getParentVersion().toString());
		assertEquals("1.2.3.CR1-v20140211-1204",  new SimpleHierarchicalVersion("1.2.3.CR1-v20140211-1204-B52").getParentVersion().toString());
		assertEquals("1.2.3.CR1-v20140211",  new SimpleHierarchicalVersion("1.2.3.CR1-v20140211-1204").getParentVersion().toString());
		assertEquals("1.2.3.CR1",  new SimpleHierarchicalVersion("1.2.3.CR1-v20140211").getParentVersion().toString());
		assertEquals("1.2.3", new SimpleHierarchicalVersion("1.2.3.CR1").getParentVersion().toString());
		assertEquals("1.2", new SimpleHierarchicalVersion("1.2.3").getParentVersion().toString());
		assertEquals("1", new SimpleHierarchicalVersion("1.2").getParentVersion().toString());
		assertNull(new SimpleHierarchicalVersion("1").getParentVersion());
	}
	
	@Test
	public void testBoundaries() {
		assertNull(new SimpleHierarchicalVersion(null).getParentVersion());
		assertNull(new SimpleHierarchicalVersion("").getParentVersion());
		assertNull(new SimpleHierarchicalVersion("garbage").getParentVersion());
	}
}
