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
package org.jboss.tools.common.ui.wizard;

import org.eclipse.jface.wizard.Wizard;

/**
 * A model base JFace wizard
 *
 */
public class AbstractModelWizard<M> extends Wizard {

	private M model;
	
	/**
	 * 
	 */
	public AbstractModelWizard(String title, M model) {
		this.model = model;
		setNeedsProgressMonitor(true);
		setWindowTitle(title);
	}

	@Override
	public boolean performFinish() {
		return true;
	}

	protected M getModel() {
		return model;
	}
}
