/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.validation.test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.wst.validation.internal.provisional.core.IMessage;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidator;
import org.jboss.tools.common.validation.ContextValidationHelper;
import org.jboss.tools.common.validation.ValidatorManager;

/**
 * @author Alexey Kazakov
 */
public class ContextValidationTest extends TestCase {

	private IProject project;

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		project = ResourcesPlugin.getWorkspace().getRoot().getProject("JavaProject");
	}

	/**
	 * https://issues.jboss.org/browse/JBIDE-15662
	 * @throws Exception
	 */
	public void testValidationContext() throws Exception {
		try {
			TestContextValidator.enable(false);
			ContextValidationHelper initialHelper = new ContextValidationHelper();
			initialHelper.setProject(project);
			IFile testJava = project.getFile("src/test/Test.java");
			initialHelper.registerResource(testJava);
			initialHelper.setValidationFileURIs(new ArrayList<String>());
			ContextValidationHelper helper = new ContextValidationHelper();
			helper.setProject(project);
			helper.setValidationFileURIs(new ArrayList<String>());
			ValidatorManager validatorManager = new ValidatorManager();
			validatorManager.validate(helper, new Reporter());
			int counter = TestContextValidator.getCounter();

			TestContextValidator.enable(true);
			testJava = project.getFile("src/test/Test2.java");
			helper = new ContextValidationHelper();
			helper.setProject(project);
			initialHelper.registerResource(testJava);
			initialHelper.setProject(project);
			helper.setValidationFileURIs(new ArrayList<String>());

			helper = new ContextValidationHelper();
			helper.setProject(project);
			helper.setValidationFileURIs(new ArrayList<String>());
			validatorManager.validate(helper, new Reporter());

			assertTrue(TestContextValidator.getCounter()>counter);
		} finally {
			TestContextValidator.enable(false);
		}
	}

	private static class Reporter implements IReporter {
		@Override
		public void addMessage(IValidator origin, IMessage message) {}
		@Override
		public void displaySubtask(IValidator validator, IMessage message) {}
		@Override
		public List getMessages() {return Collections.EMPTY_LIST;}
		@Override
		public boolean isCancelled() {return false;}
		@Override
		public void removeAllMessages(IValidator origin) {}
		@Override
		public void removeAllMessages(IValidator origin, Object object) {}
		@Override
		public void removeMessageSubset(IValidator validator, Object obj, String groupName) {}
	}
}