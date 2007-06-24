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

import java.util.*;
import org.jboss.tools.common.model.markers.*;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.markers.ResourceProblems;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.util.PositionHolder;
import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.verification.vrules.layer.VObjectImpl;

public class VTaskListenerImpl implements VTaskListener {
	protected VTask task;
	protected XModel model;
	protected int errors = 0;	
	protected Map<XModelObject,ResourceProblems> problemList = new HashMap<XModelObject,ResourceProblems>();
	protected int significance = 0;
	
	public void setModel(XModel model) {
		this.model = model; 
	}
	
	public void setTask(VTask task) {
		this.task = task;
	}
	
	public void setSignificance(int significance) {
		this.significance = significance;
	}

	public void onStart() {
		errors = 0;
		problemList.clear(); 
	}

	public void onRuleApplied(final VRule rule, final VObject object, final VResult[] results) {
		if(!rule.isEnabled()) return;
		if(results != null) {
			for (int i = 0; i < results.length; i++) {
				if(results[i] == null) {
///					ModelPlugin.log("Result in rule " + rule.getName() + " is null.");
					continue;
				}
				if (results[i].getSignificance() <= significance) continue;
				++errors;
				if(getErrorCountLimit() >= 0 && errors > getErrorCountLimit()) {
					if(errors == getErrorCountLimit() + 1) {
						task.stop();
					} 
				} else {
					addProblem(rule, object, results[i]);
				}
			}
		} else addProblem(rule, object, null); 
	}
	
	public void onRuleFinished(final VRule rule, VObject object) {
		onRuleApplied(rule, object, null);
	}
        
	public void onPause() {}

	public void onResume() {}

	public void onFinish() {
		addMarkers();
		task.removeTaskListener(this);
	}

	protected void addProblem(VRule rule, VObject object, VResult result) {
		VObjectImpl oi = (VObjectImpl)object;
		XModelObject o = (oi == null) ? null : oi.getModelObject();
		if(o == null) return;
		XModelObject f = o;
		while(f != null && f.getFileType() != XModelObject.FILE) f = f.getParent();
		if(!(f instanceof FileAnyImpl)) return;
		ResourceProblems p = (ResourceProblems)problemList.get(f);
		String msg = (result == null) ? null : result.getMessage();
		if(p == null) {
			problemList.put(f, p = new ResourceProblems(f));
		}
		Object sp = result == null ? null : result.getSourcePosition();
		String attr = sp == null ? null : sp.toString();
		if(msg != null) {
			PositionHolder h = PositionHolder.getPosition(o, attr);
			h.update();
			p.addError(o.getPath(), msg, attr, h);
		} 
	}
	
	protected void addMarkers() {
		Iterator it = problemList.values().iterator();
		while(it.hasNext()) {
			ResourceProblems p = (ResourceProblems)it.next();
			ResourceMarkers markers = new VerificationMarkers(p);
			markers.setModelObject(p.getResourceObject());
			markers.update();
		}
	}
	
	int limit = -2;
	
	protected int getErrorCountLimit() {
		if(limit != -2) return limit;
		String path = "%Options%/Struts Studio/Verification";
		String attr = "Reported Errors Number Limit";
		XModelObject o = PreferenceModelUtilities.getPreferenceModel().getByPath(path);
		if(o == null) return limit = -1;
		String s = o.getAttributeValue(attr);
		limit = -1;
		if(!"unlimited".equals(s)) try {
			limit = Integer.parseInt(s);
		} catch (Exception e) {}
		return limit;		
	}	
	
}
	