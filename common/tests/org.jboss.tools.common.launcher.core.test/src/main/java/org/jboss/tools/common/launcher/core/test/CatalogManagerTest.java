/*************************************************************************************
 * Copyright (c) 2018 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.launcher.core.test;

import static org.junit.Assert.assertNotNull;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.launcher.core.model.CatalogManager;
import org.junit.BeforeClass;
import org.junit.Test;

public class CatalogManagerTest {
	private static CatalogManager manager;
	
	@BeforeClass
	public static void setup() {
		manager = CatalogManager.getDefault();
	}
	
	@Test
	public void testThatManagerIsAvailable() {
		assertNotNull(manager);
	}
	
	@Test
	public void testThatManagerReturnsDefaultCatalog() throws CoreException  {
		assertNotNull(manager.getCatalog(new NullProgressMonitor()));
	}
	
}
