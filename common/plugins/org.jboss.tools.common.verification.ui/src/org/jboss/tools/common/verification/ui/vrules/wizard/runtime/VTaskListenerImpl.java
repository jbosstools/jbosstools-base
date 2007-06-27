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
package org.jboss.tools.common.verification.ui.vrules.wizard.runtime;

import java.util.*;
import org.jboss.tools.common.model.markers.ResourceMarkers;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.markers.ResourceProblems;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.verification.ui.XStudioVerificationPlugin;
import org.jboss.tools.common.verification.vrules.core.resources.VerificationMarkers;
import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.verification.vrules.layer.VObjectImpl;

public class VTaskListenerImpl implements VTaskListener {
	private VerifyWizardView view;
	int errors = 0;
	Map<XModelObject,ResourceProblems> problemList = new HashMap<XModelObject,ResourceProblems>();
	
	public VTaskListenerImpl(VerifyWizardView view) {
		this.view = view;
	}

	public void onStart() {
		view.changeControl(VerifyWizardView.RUNNING_COMMANDS);
		errors = 0; 
	}

	public void onRuleApplied(final VRule rule, final VObject object, final VResult[] results) {
		if(!rule.isEnabled()) return;
		RuntimeItemWrapper w = view.findWrapper(rule.getName());
		RuntimeRuleSetWrapper rsw = (RuntimeRuleSetWrapper)view.findWrapper(rule.getRuleSet().getName());
		int errorCount = 0;
		if(results != null) {
			for (int i = 0; i < results.length; i++) {
				if(results[i] == null) {
					XStudioVerificationPlugin.getPluginLog().logInfo("Result in rule " + rule.getName() + " is null.");
					continue;
				}
				if (results[i].getSignificance() <= VHelper.getManager(/*view.model*/).getMinSignificance()) continue;
///				view.model.getOut().println(rule.getName() + ": " + results[i].getMessage());
				++errorCount;
				++errors;
				if(view.getErrorCountLimit() >= 0 && errors > view.getErrorCountLimit()) {
					if(errors == view.getErrorCountLimit() + 1) view.limitReached();
				} else {
					addProblem(rule, object, results[i]);
				}
			}
		} else addProblem(rule, object, null); 
		if (errorCount == 0) {
			if(w != null) w.mergeStatus(1);
			if(rsw != null) rsw.updateStatus();
		} else {
			if(w != null) {
				w.mergeStatus(2);
				if(rsw != null) rsw.mergeStatus(2);
			}
		}
	}

	public void onRuleFinished(final VRule rule, VObject object) {
		onRuleApplied(rule, object, null);
	}
        
	public void onPause() {
		view.changeControl(VerifyWizardView.PAUSE_COMMANDS);
	}

	public void onResume() {
		view.changeControl(VerifyWizardView.RUNNING_COMMANDS);
	}

	public void onFinish() {
		view.changeControl(VerifyWizardView.INITIAL_COMMANDS);
		addMarkers();
	}

	void addProblem(VRule rule, VObject object, VResult result) {
		VObjectImpl oi = (VObjectImpl)object;
		XModelObject o = (oi == null) ? null : oi.getModelObject();
		if(o == null) return;
		XModelObject f = o;
		while(f != null && f.getFileType() != XModelObject.FILE) f = f.getParent();
		if(f == null) return;
		ResourceProblems p = (ResourceProblems)problemList.get(f);
		String msg = (result == null) ? null : result.getMessage();
		if(p == null) {
			problemList.put(f, p = new ResourceProblems(f));
		}
		Object sp = result == null ? null : result.getSourcePosition();
		String attr = sp == null ? null : sp.toString();
		if(msg != null) p.addError(o.getPath(), msg, attr, -1);		
	}
	
	void addMarkers() {
		Iterator it = problemList.values().iterator();
		while(it.hasNext()) {
			ResourceProblems p = (ResourceProblems)it.next();
			ResourceMarkers markers = new VerificationMarkers(p);
			markers.setModelObject(p.getResourceObject());
			markers.update();
		}
	}
	
}

