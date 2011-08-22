/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation.test;

import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.jboss.tools.common.base.test.validation.TestUtil;
import org.jboss.tools.common.base.test.validation.ValidationExceptionTest;
import org.jboss.tools.common.validation.CommonValidationPlugin;
import org.jboss.tools.common.validation.JBTValidationException;

/**
 * @author Alexey Kazakov
 */
public class ValidationTest extends ValidationExceptionTest {

	@Override
	public void testExceptions() throws Exception {
		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject("JavaProject");
		TestUtil._waitForValidation(project);
		Set<IStatus> exceptions = LOGGER.getExceptions();
		assertFalse(exceptions.isEmpty());
	}

	@Override
	public void testLogger() {
		initLogger();
		CommonValidationPlugin.getDefault().logError(new JBTValidationException("Test logger", null));
		Set<IStatus> exceptions = LOGGER.getExceptions();
		assertEquals(1, exceptions.size());
	}
}