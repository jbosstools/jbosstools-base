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
package org.jboss.tools.common.model.ui.wizard.newfile;

import org.jboss.tools.common.model.ui.ModelUIImages;

public class NewPropertiesFileWizard extends NewFileWizardEx {
	
	public NewPropertiesFileWizard(){
		super();
		setDefaultPageImageDescriptor(ModelUIImages.getInstance().getOrCreateImageDescriptor(ModelUIImages.PROPERTIES_FILE_IMAGE));
	}

	protected NewFileContextEx createNewFileContext() {
		return new NewPropertiesFileContext();
	}
	
	class NewPropertiesFileContext extends NewFileContextEx {
		protected String getActionPath() {
			return "CreateActions.CreateFiles.Common.CreateFileProperties"; //$NON-NLS-1$
		}
	}

}
