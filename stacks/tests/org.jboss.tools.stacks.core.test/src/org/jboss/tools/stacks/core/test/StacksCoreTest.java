/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.stacks.core.test;

/*************************************************************************************
 * Copyright (c) 2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
import junit.framework.TestCase;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.jdf.stacks.model.Stacks;
import org.jboss.tools.stacks.core.model.StacksManager;
import org.junit.Test;

public class StacksCoreTest extends TestCase {
	private static final String STACKS_URL_KEY = "org.jboss.examples.stacks.url";

	@Test
	public void testManagerFindsStacks() {
		try {
			Stacks s = new StacksManager().getStacks(new NullProgressMonitor());
			assertNotNull(s);
		} catch(Throwable t) {
			throw new RuntimeException(t);
		}
	}
	
	@Test
	public void testManagerUseDefaultStacks() {
		try {
			System.setProperty(STACKS_URL_KEY, "http://invalid.stacks.url");
			Stacks s = new StacksManager().getStacks(new NullProgressMonitor());
			assertNotNull(s);
		} finally {
			System.clearProperty(STACKS_URL_KEY);
		}
	}
	
}
