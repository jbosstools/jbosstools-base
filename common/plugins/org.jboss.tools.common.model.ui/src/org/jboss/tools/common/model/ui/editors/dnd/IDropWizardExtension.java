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
package org.jboss.tools.common.model.ui.editors.dnd;

/**
 * Extension to be implemented in drop wizards that should be invoked
 * without dialog.
 * 
 * @author Viacheslav Kabanovich
 *
 */
public interface IDropWizardExtension {

	/**
	 * Initializes internal model without UI context.
	 * This method should be invoked after setCommand(IDropCommand).
	 * After thismethod is invoked succesfully, methods 
	 * performFinish() and getWizardModel().getElementGenerator() 
	 * can be invoked.
	 * 
	 */
	public void initWithoutUI();

}
