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
package org.jboss.tools.common.verification.vrules.core.resources;

import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.verification.vrules.layer.VModelFactory;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;

public class GlobalBuilderImpl {
	protected XModel model;

	private VManager getRulesManager() {
		return VHelper.getManager();
	}
	
	public void setModel(XModel model) {
		this.model = model;
	}
	
	public void execute(XModelObject object) {
		if(object == null) object = FileSystemsHelper.getWebInf(model);
		if(object == null) return;
		VModel vmodel = VModelFactory.getModel(object.getModel());
		VObject vobject = vmodel.getObjectByPath(object.getPath());
		if(getRulesManager() == null) {
			return;
		}
		VRule[] rules = VHelper.getRules(getRulesManager(), vobject);
		if(rules == null) return;
		VTask task = getRulesManager().createTask(vobject);
		VTaskListenerImpl listener = new VTaskListenerImpl();
		listener.setModel(model);
		listener.setTask(task);
		listener.setSignificance(getRulesManager().getMinSignificance());
		task.addTaskListener(listener);
		task.run();
		task.removeTaskListener(listener);
	}
	
}
