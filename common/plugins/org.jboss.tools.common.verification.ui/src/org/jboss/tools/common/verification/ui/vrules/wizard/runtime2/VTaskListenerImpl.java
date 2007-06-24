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
package org.jboss.tools.common.verification.ui.vrules.wizard.runtime2;

import org.jboss.tools.common.verification.vrules.*;

public class VTaskListenerImpl extends org.jboss.tools.common.verification.vrules.core.resources.VTaskListenerImpl {
	private VerifyWizardView view;
	
	public VTaskListenerImpl(VerifyWizardView view) {
		this.view = view;
	}
	
	long startTime = -1;

	public void onStart() {
		super.onStart();
		startTime = System.currentTimeMillis();
	}

	public void onRuleFinished(VRule rule, VObject object) {
		onRuleApplied(rule, object, null);
		view.onRuleFinished(object);
	}
        
	public void onFinish() {
		addMarkers();
		long endTime = System.currentTimeMillis();
		long delta = 500 + startTime - endTime;
		if(delta > 0) try { Thread.sleep(delta); } catch (Exception e) {}
		view.onFinish();
		try { Thread.sleep(200); } catch (Exception e) {}
		view.action("Close");
		task.removeTaskListener(this);
	}

}
