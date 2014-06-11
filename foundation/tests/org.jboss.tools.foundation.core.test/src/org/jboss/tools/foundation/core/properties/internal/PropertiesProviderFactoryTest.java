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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.jboss.tools.foundation.core.properties.IPropertiesProvider;
import org.jboss.tools.foundation.core.properties.mock.MockVersionProvider;
import org.junit.Test;

public class PropertiesProviderFactoryTest {
	
	@Test
	public void testLoadPropertiesProviders() throws Exception {
		PropertiesProviderFactory factory = new PropertiesProviderFactory();
		List<IPropertiesProvider> providers = factory.loadPropertiesProviders();
		assertNotNull(providers);
		assertTrue(providers.toString(), providers.size() > 1);
		
		//Lowest priority provider should come last
		assertEquals("jbosstools.properties.provider", ((VersionPropertiesProvider)providers.get(providers.size() -2)).getId());
		assertEquals(MockVersionProvider.class, providers.get(providers.size() -1).getClass());
	}

}
