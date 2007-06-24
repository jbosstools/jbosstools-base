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

import java.util.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.handlers.MoveHandler;
import org.jboss.tools.common.model.*;

public class MoveFileSystemHandler extends MoveHandler {
	static SpecialWizard wizard = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.struts.webprj.model.helpers.sync.SortFileSystems");
	
	public void executeHandler(XModelObject object, Properties prop) throws Exception {
		if(!isEnabled(object)) return;
		super.executeHandler(object, prop);
		sortFileSystems(object.getModel());
	}
	
	public static void sortFileSystems(XModel model) {
		if(wizard != null) {
			wizard.setObject(model);
			wizard.execute();
		}		
	}
	
}
