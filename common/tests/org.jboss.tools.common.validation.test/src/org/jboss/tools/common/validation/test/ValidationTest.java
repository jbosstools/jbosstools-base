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

import junit.framework.TestCase;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.jboss.tools.common.base.test.validation.TestUtil;
import org.jboss.tools.common.base.test.validation.ValidationExceptionLogger;
import org.jboss.tools.common.base.test.validation.ValidationExceptionTest;

/**
 * @author Alexey Kazakov
 */
public class ValidationTest extends TestCase {

	public void testExceptions() throws Exception {
		ValidationExceptionLogger logger = new ValidationExceptionLogger();
		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject("JavaProject");
		TestUtil._waitForValidation(project);
		Set<IStatus> exceptions = logger.getExceptions();
		assertFalse(exceptions.isEmpty());
	}

	public void testLogging() {
		new ValidationExceptionTest().testLogging();
	}
}