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
package org.jboss.tools.common.verification.test;

import junit.framework.Assert;

import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.verification.vrules.VHelper;
import org.jboss.tools.common.verification.vrules.VManager;
import org.jboss.tools.common.verification.vrules.VModel;
import org.jboss.tools.common.verification.vrules.VObject;
import org.jboss.tools.common.verification.vrules.VResult;
import org.jboss.tools.common.verification.vrules.VRule;
import org.jboss.tools.common.verification.vrules.VTask;
import org.jboss.tools.common.verification.vrules.VTaskListener;
import org.jboss.tools.common.verification.vrules.layer.VModelFactory;

public class VerificationUtil {

	public static VResult[] doTestVerification(XModelObject o) {
		XModel model = o.getModel();
		String entity = o.getModelEntity().getName();
		VManager m = VHelper.getManager();
		VModel vmodel = VModelFactory.getModel(model);
		Assert.assertTrue("Cannot obtain VModel " + entity, vmodel != null);
		
		VObject vo = vmodel.getObjectByPath(o.getPath());
		Assert.assertTrue("Cannot obtain VObject for test object " + entity + ":" + o.getPath(), vo != null);
		
		VRule[] rules = VHelper.getRules(m, vo);
		Assert.assertTrue("Cannot find rules for test object " + entity + ":" + o.getPath(), rules != null && rules.length > 0);

		VTask task = m.createTask(vo);
		VTaskListenerImpl listener = new VTaskListenerImpl();
		task.addTaskListener(listener);
		task.run();
		task.removeTaskListener(listener);
		
		return listener.results;
	}
	
	static class VTaskListenerImpl implements VTaskListener {
		boolean finished = false;
		VResult[] results = null;
		
		
		public VTaskListenerImpl() {
		}

		public void onFinish() {
			finished = true;
		}

		public void onPause() {
		}

		public void onResume() {			
		}

		public void onRuleApplied(VRule rule, VObject object, VResult[] results) {
			if(results != null && results.length > 0) {
				this.results = results;
			}
		}

		public void onRuleFinished(VRule rule, VObject object) {}

		public void onStart() {}
		
	}

}
