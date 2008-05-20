/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.wizards;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.ui.wizards.NewClassWizardPage;

/**
 * @author au
 */
public class NewClassWizardPageEx extends NewClassWizardPage {
	
	private NewTypeWizardAdapter adapter = null;
	
	public NewClassWizardPageEx() {
		super();
	}

	public void init(NewTypeWizardAdapter adapter) {
		this.adapter = adapter;
		setPackageFragmentRoot(adapter.getPackageFragmentRoot(), adapter.isCanBeModified());
		setPackageFragment(adapter.getPackageFragment(), adapter.isCanBeModified());
		setEnclosingType(adapter.getEnclosingType(), adapter.isCanBeModified());
		setEnclosingTypeSelection(adapter.getEnclosingTypeSelection(), adapter.isCanBeModified());
		setTypeName(adapter.getTypeName(), adapter.isCanBeModified());
		if (adapter.getSuperClass()!=null && adapter.getSuperClass().length()>0) {
			setSuperClass(adapter.getSuperClass(), adapter.isCanBeModified());
		}
		if (adapter.getSuperInterfaces()!=null) {
			setSuperInterfaces(adapter.getSuperInterfaces(), adapter.isCanBeModified());
		}
		setMethodStubSelection(false, adapter.isCreateConstructors(), 
				adapter.isCreateInherited(), adapter.isCanBeModified());
	}
	
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		// policy: wizards are not allowed to come up with an error message;
		// in this wizard, some fields may need initial validation and thus,
		// potentially start with an error message.
		IStatus classNameStatus = adapter.getClassNameStatus();
		if (classNameStatus !=null && !classNameStatus.isOK()) updateStatus(classNameStatus);
		IStatus packageNameStatus = adapter.getPackageNameStatus();
		if (packageNameStatus != null && !packageNameStatus.isOK())	updateStatus(packageNameStatus);
	}
}
