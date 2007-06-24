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
package org.jboss.tools.common.model.filesystems.impl;

import java.util.Properties;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.XFileObject;

public class DeleteFileHandler extends AbstractHandler {
	SpecialWizard closeEditor = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.editor.DeleteFileUnderEditingSpecialWizard");

	public boolean isEnabled(XModelObject object) {
		return object != null && object.isActive() && object.getFileType() > XFileObject.NONE
		       && (object.getParent() instanceof FolderImpl);
	}

	public void executeHandler(XModelObject object, Properties p) throws Exception {
		if(!isEnabled(object)) return;
		object.setModified(false);
		if(closeEditor != null) {
			closeEditor.setObject(object);
			closeEditor.execute();
		}
		FolderImpl parent = (FolderImpl)object.getParent();
		parent.removeChildFile(object); 
	}
	
	public boolean getSignificantFlag(XModelObject object) {
		return true;
	}

}
