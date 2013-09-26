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
package org.jboss.tools.common.ui.propertytester.internal;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.part.FileEditorInput;
import org.jboss.tools.common.CommonPlugin;

public class NaturePropertyTester extends PropertyTester {
	public static final String PROPERTY_NAME = "matchesNature"; //$NON-NLS-1$

	@Override
	/**
	 * method looks for nature for given IEditorPart
	 * 
	 * @parameters
	 * receiver - IEditorPart
	 * property - "matchesNature"
	 * expectedValue - project nature id
	 */
	public boolean test(Object receiver, String property, Object[] args, Object expectedValue) {
		if(receiver instanceof IEditorPart &&
				PROPERTY_NAME.equals(property) &&
				expectedValue instanceof String){
			
			IEditorPart editor = (IEditorPart)receiver;
			String nature = (String)expectedValue;
			
			IEditorInput input = editor.getEditorInput();
			if(input instanceof FileEditorInput){
				IProject project =  ((FileEditorInput)input).getFile().getProject();
				if(project != null && project.exists()){
					try {
						return project.hasNature(nature);
					} catch (CoreException e) {
						CommonPlugin.getDefault().logError(e);
					}
				}
			}
		}
		return false;
	}

}
