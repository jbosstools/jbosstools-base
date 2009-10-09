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
package org.jboss.tools.common.meta.action.impl;

import java.util.*;

import org.jboss.tools.common.model.*;
import org.eclipse.swt.widgets.Display;
import org.jboss.tools.common.meta.XMapping;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.handlers.*;
import org.jboss.tools.common.meta.help.HelpUtil;
import org.jboss.tools.common.meta.key.*;

public abstract class SpecialWizardSupport {
    public static final String OK = Messages.SpecialWizardSupport_OK;
    public static final String CANCEL = Messages.SpecialWizardSupport_Cancel;
    public static final String BACK = Messages.SpecialWizardSupport_BackArrow;
    public static final String NEXT = Messages.SpecialWizardSupport_NextArrow;
    public static final String FINISH = Messages.SpecialWizardSupport_Finish;
	public static final String HELP = Messages.SpecialWizardSupport_Help;
	public static final String CLOSE = Messages.SpecialWizardSupport_Close;
	
	static final Set<String> standardButtons = new HashSet<String>();
	
	static {
		standardButtons.add(OK);
		standardButtons.add(CANCEL);
		standardButtons.add(BACK);
		standardButtons.add(NEXT);
		standardButtons.add(FINISH);
		standardButtons.add(HELP);
		standardButtons.add(CLOSE);
	}

    private int stepId = 0;
    private boolean finished = false;
    private XEntityData[] data = null;
    protected XAction action = null;
    protected XModelObject target;
    protected Properties p;
    private SpecialWizardControlListener listener = null;

    public SpecialWizardSupport() {}
    
    public void dispose() {
    	/*
    	if (p!=null) p.clear();
    	p = null;
    	listener = null;
    	*/
    }
    
    public final void setAction(XAction action) {
    	this.action = action;
    }

    public final void setActionData(XAction action, XEntityData[] data, XModelObject target, Properties p) {
        this.action = action;
        this.data = data;
        this.target = target;
        this.p = (p != null) ? p : new Properties();
        stepId = 0;
        finished = false;
        reset();
    }
    
    public void help() throws XModelException {
    	HelpUtil.helpEclipse(getTarget().getModel(), getHelpKey());
    }

    protected void reset() {}

    public final XEntityData[] getEntityData() {
        return data;
    }

    public String getTitle() {
    	String title = WizardKeys.getHeader(getHelpKey());
    	if(title != null) return title;
    	title = WizardKeys.getHeader(action, getEntityData()[0].getModelEntity()); // no step id
    	if(title != null) return title;
		String n = (action == null) ? "" : "" + action.getDisplayName(); //$NON-NLS-1$ //$NON-NLS-2$
		if(n.endsWith("...")) n = n.substring(0, n.length() - 3); //$NON-NLS-1$
		return n;
    }
    
    public String getSubtitle() {
    	String key = getHelpKey();
    	String t = WizardKeys.getTitle(key);
    	if(t == null && key != null && key.endsWith("_0")) { //$NON-NLS-1$
    		String key1 = key.substring(0, key.length() - 2);
    		t = WizardKeys.getTitle(key1);
    	}
    	if(t == null) {
    		t = WizardKeys.getTitle(action, getEntityData()[0].getModelEntity()); // no step id
    	}
		return t;
    }

    public boolean isEnabled(XModelObject target) {
        return target != null && target.isObjectEditable();
    }

    public String getMessage(int stepId) {
        return WizardKeys.getMessage(getHelpKey());
    }

    /*
     * May return preset or previosly entered data (name-value)
     */
    public String[][] getInfo(int stepId) {
        return new String[0][0];
    }

    /*
     * Returns user friendly display name or message by attribute
     * internal name. If null returned, attrname is used as display name.
     */
    public String getAttributeMessage(int stepId, String attrname) {
        return null;
    }

    public void setControlListener(SpecialWizardControlListener listener) {
        this.listener = listener;
    }

    public void fireCommand(final String command) {
        if(listener != null) {
			Display.getDefault().syncExec( 
				new Runnable() {
					public void run() {
						listener.action(command);
					}
				}
			);
        }
    }

    public int getStepId() {
        return stepId;
    }

    public final void setStepId(int stepId) {
        this.stepId = stepId;
    }

    public final boolean isFinished() {
        return finished;
    }

    public final void setFinished(boolean b) {
        finished = b;
    }

    public abstract void action(String name) throws XModelException;

    public String[] getActionNames(int stepId) {
        return new String[]{OK, CANCEL, HELP};
    }
    
    public boolean canBeProcessedByStandardWizard() {
    	// By default only one-step wizards are accepted;
    	if(data == null || data.length > 1) return false;
    	if(!DEFAULT_IMLEMENTING_CLASS.equals(getStepImplementingClass(0))) {
    		return false;
    	}
    	String[] buttons = getActionNames(0);
    	for (int i = 0; i < buttons.length; i++) {
    		if(!standardButtons.contains(buttons[i])) return false;
    	}
    	return true;
    }

    public String getDefaultActionName(int stepId) {
        String[] actions = getActionNames(stepId);
        if(actions == null || actions.length == 0) return null;
        Set<String> set = new HashSet<String>();
        for (int i = 0; i < actions.length; i++) set.add(actions[i]);
        if(set.contains(OK)) return OK;
        if(set.contains(FINISH)) return FINISH;
        if(set.contains(NEXT)) return NEXT;
        return null;
    }

    private Thread thread = null;

    public final void startThread(Runnable task) {
        if(thread != null) stopThread(true);
        thread = new Thread(task);
        thread.start();
    }

    public final void stopThread(boolean force) {
        if(thread == null) return;
        if(thread.isAlive() && force) {
            	thread.stop();
        }
        thread = null;
    }

    public String getHelpKey() {
    	if(action == null) return null;
    	String key = action.getProperty("key"); //$NON-NLS-1$
    	if(key != null && key.length() > 0) return key;
        return target.getModelEntity().getName() + "_" + action.getName() + "_" + stepId; //$NON-NLS-1$ //$NON-NLS-2$
    }

    public final XModelObject getTarget() {
        return target;
    }

    public final Properties getProperties() {
    	if (p == null) p = new Properties();
        return p;
    }

    public boolean isActionEnabled(String name) {
        return true;
    }
    
    public boolean isFieldEditorEnabled(int stepId, String name, Properties values) {
    	return true;
    }
    
    protected DefaultWizardDataValidator defaultValidator = new DefaultWizardDataValidator();
    
    public WizardDataValidator getValidator(int step) {
		defaultValidator.setSupport(this, step);
		return defaultValidator;    	
    }
    
    static String DEFAULT_IMLEMENTING_CLASS = "org.jboss.tools.common.model.ui.wizards.special.SpecialWizardStep"; //$NON-NLS-1$

    public String getStepImplementingClass(int stepId) {
        return DEFAULT_IMLEMENTING_CLASS;
    }

    // helpers

    public Properties extractStepData(int index) throws RuntimeException {
    	XEntityData d = getEntityData()[index];
    	Properties p = DefaultCreateHandler.getProperties(d);
		XAttributeData[] ads = d.getAttributeData();
		for (int i = 0; i < ads.length; i++) {
			String n = ads[i].getAttribute().getName();
			if(!isFieldEditorEnabled(index, n, p)) continue;
			DefaultCreateHandler.extractProperty(ads[i]);
		}
        return p;
    }
    
    public void initStepData(int index, XModelObject object) {
    	XEntityData d = getEntityData()[index];
        XAttributeData[] ad = d.getAttributeData();
        for (int i = 0; i < ad.length; i++) {
            String n = ad[i].getAttribute().getName();
            String v = object.getAttributeValue(n);
            if(v != null) ad[i].setValue(v);
            v = action.getProperty("attribute." + n); //$NON-NLS-1$
            if(v != null) ad[i].setValue(v);
        }
    }

    public final XAttributeData findAttribute(int index, String name) {
        return HUtil.find(getEntityData(), index, name);
    }

    public final String getAttributeValue(int index, String name) {
        XAttributeData d = findAttribute(index, name);
        return (d != null) ? d.getValue() : null;
    }

    public final void setAttributeValue(int index, String name, String value) {
        XAttributeData d = findAttribute(index, name);
        if(d != null) d.setValue(value);
    }

    public final void setValueList(int index, String name, String[] values) {
        HUtil.hackAttributeConstraintList(getEntityData(), index, name, values);
    }

    public final void setAttributeContext(int index, String name, Object context) {
        XAttributeData d = findAttribute(index, name);
        if(d != null) d.getAttribute().getEditor().setContext(context);
    }
    
    public final void setAttributeDataByObject(int index, XModelObject o) {
		XAttributeData[] ad = getEntityData()[0].getAttributeData();
		for (int i = 0; i < ad.length; i++) {
			String n = ad[i].getAttribute().getName();
			String v = o.getAttributeValue(n);
			if(v != null) setAttributeValue(0, n, v);
		}
    }
    
    protected final void replaceEntityData(XEntityData d, int stepId) {
    	if(data[stepId] == d) return;
    	XAttributeData[] ad = d.getAttributeData();
    	for (int i = 0; i < ad.length; i++) {
    		String n = ad[i].getAttribute().getName();
    		String v = getAttributeValue(stepId, n);
    		if(v == null) v = ad[i].getAttribute().getDefaultValue();
    		if(v != null) ad[i].setValue(v);    		
    	}
    	data[stepId] = d;
    }

	public int getPreviousStepId() {
		return getStepId() - 1;	
	}
	
	public String getFocusAttribute(int stepId) {
		XEntityData[] ds = getEntityData();
		if(ds == null || stepId < 0 || stepId >= ds.length) return null;
		XEntityData d = ds[stepId];
		XAttributeData[] ad = d.getAttributeData();
		if(ad.length == 0) return null;
		XAttributeData best = null;
		int importance = -1;
		for (int i = 0; i < ad.length && importance < 3; i++) {
			int imp = (ad[i].getMandatoryFlag()) ? 1 : 0;
			imp += (ad[i].getValue() == null || ad[i].getValue().length() == 0) ? 2 : 0;
			if(imp > importance) {
				best = ad[i];
				importance = imp;
			}
		}
		return best == null ? null : best.getAttribute().getName();
	}
	
	public String getHelpContextId() {
		if(action == null) return null;
		String s = action.getProperty("helpId"); //$NON-NLS-1$
		if(s == null || s.length() == 0) return null;
		int i = s.indexOf('%');
		if(i < 0) return s;
		int j = s.indexOf('%', i + 1);
		if(j < 0) return s;
		String vr = s.substring(i + 1, j);
		String tail = s.substring(j + 1);
		XMapping map = action.getMetaModel().getMapping("HelpPlugins"); //$NON-NLS-1$
		if(map == null) return s;
		String varValue = map.getValue(vr);
		if(varValue == null) return s;
		return varValue + tail;
	}

}

