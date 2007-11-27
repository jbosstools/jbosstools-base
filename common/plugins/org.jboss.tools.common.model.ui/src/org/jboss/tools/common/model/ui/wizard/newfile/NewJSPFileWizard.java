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

import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.model.files.handlers.CreateFileSupport;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public class NewJSPFileWizard extends NewFileWizardEx {

	protected NewFileContextEx createNewFileContext() {
		return new NewJSPFileContext();
	}
	
	class NewJSPFileContext extends NewFileContextEx {
		protected SpecialWizardSupport createSupport() {
			CreateFileSupport support = null;
			try {
				support = (CreateFileSupport)ModelFeatureFactory.getInstance().createFeatureInstance("org.jboss.tools.jst.web.model.handlers.CreateJSPFileSupport");
			} catch (Exception e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
			if(support == null) {
				support = new CreateFileSupport();
			}
			return support;
		}
		protected String getActionPath() {
			return "CreateActions.CreateFiles.Web.CreateFileJSP";
		}
	}
		
}
