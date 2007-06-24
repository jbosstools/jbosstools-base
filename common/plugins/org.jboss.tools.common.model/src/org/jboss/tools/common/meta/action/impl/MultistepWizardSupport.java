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

import java.util.Properties;
import org.jboss.tools.common.meta.action.impl.handlers.PasteHandler;

public class MultistepWizardSupport extends SpecialWizardSupport {
	protected MultistepWizardStep[] steps = createSteps();
	
	protected MultistepWizardStep[] createSteps() {
		return new MultistepWizardStep[0];		
	}

	protected void initSteps() {
		for(int i = 0; i < steps.length; i++) {
			steps[i].setSupport(this, i);
		}
	}

	protected int[] previousSteps = new int[10];

	public void action(String name) throws Exception {
		if(FINISH.equals(name)) {
			execute();
			setFinished(true);
		} else if(CANCEL.equals(name)) {
			getProperties().setProperty(PasteHandler.IS_CANCELLED, "true");
			setFinished(true);
		} else if(NEXT.equals(name)) {
			int next = next();
			previousSteps[next] = getStepId();
			setStepId(next);
		} else if(BACK.equals(name)) {
			setStepId(previousSteps[getStepId()]);
		} else if(HELP.equals(name)) {
			help();
		}
	}

	public int getPreviousStepId() {
		return previousSteps[getStepId()];	
	}
	
	static String[] N_F_C_ACTIONS = new String[]{NEXT, FINISH, CANCEL, HELP};
	static String[] F_C_ACTIONS = new String[]{FINISH, CANCEL, HELP};
	static String[] B_F_C_ACTIONS = new String[]{BACK, FINISH, CANCEL, HELP};
	static String[] B_N_F_C_ACTIONS = new String[]{BACK, NEXT, FINISH, CANCEL, HELP};
	
	public String[] getActionNames(int stepId) {
		if(stepId == 0) {
			return (hasNext(stepId) ? N_F_C_ACTIONS : F_C_ACTIONS);
		} else {
			return (hasNext(stepId) ? B_N_F_C_ACTIONS : B_F_C_ACTIONS);
		} 
	}
	
	protected boolean hasNext(int stepId) {
		return stepId < steps.length - 1; 
	}
	
	public String getStepImplementingClass(int stepId) {
		if(stepId >= 0 && stepId < steps.length) return steps[stepId].getStepImplementingClass();
		return super.getStepImplementingClass(stepId);
	}

	public boolean isFieldEditorEnabled(int stepId, String name, Properties values) {
		return steps[stepId].isFieldEditorEnabled(name, values);
	}    
	
	//override
	
	private int next() throws Exception {
		int step = getStepId() + 1;
		while(!isRequired(step)) ++step;
		prepareStep(step);
		return step;
	}
	
	protected void prepareStep(int nextStep) throws Exception {		
	}

	protected boolean isRequired(int nextStep) {
		return true;
	}

	protected void execute() throws Exception {}

}
