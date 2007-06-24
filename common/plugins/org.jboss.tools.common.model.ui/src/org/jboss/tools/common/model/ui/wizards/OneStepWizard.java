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

import java.util.Properties;
import org.jboss.tools.common.model.ui.wizards.special.DefaultSpecialWizard;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.model.XModelObject;

public class OneStepWizard implements SpecialWizard {
	Properties p; 
	XAction action;
	XModelObject modelObject;

	public OneStepWizard() {}

	public void setObject(Object object) {
		p = (Properties)object;
		action = (XAction)p.get("action");
		modelObject = (XModelObject)p.get("object");
	}
	
	public int execute() {
		SpecialWizardSupport support = new SpecialWizardSupportOneImpl();
		support.setActionData(action, action.getEntityData(modelObject), modelObject, p);
		DefaultSpecialWizard w = new DefaultSpecialWizard();
		w.setObject(new Object[]{support});
		w.execute();
		return 0;
	}

}

class SpecialWizardSupportOneImpl extends SpecialWizardSupport {

	public String[] getActionNames(int stepId) {
		return new String[]{FINISH, CANCEL, HELP};
	}
		
	public void action(String name) throws Exception {
		if(CANCEL.equals(name)) {
			setFinished(true);
		} else if(FINISH.equals(name)) {
			action.executeHandler(getTarget(), p);
			setFinished(true);
		}
	}
	
	public String getHelpKey() {
		if(action == null) return null;
		return target.getModelEntity().getName() + "_" + action.getName().replace(' ', '_');
	}

}
