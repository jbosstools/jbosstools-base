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

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.internal.ui.*;
import org.eclipse.jdt.internal.ui.actions.*;
import org.eclipse.jdt.internal.ui.wizards.*;
import org.eclipse.jface.wizard.Wizard;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

/**
 * @author au
 */

public class NewClassWizard extends Wizard {

	protected NewTypeWizardAdapter adapter = null;
	protected NewClassWizardPageEx mainPage;
	
	public NewClassWizard() {
		setDialogSettings(JavaPlugin.getDefault().getDialogSettings());
		setWindowTitle(NewWizardMessages.NewClassCreationWizard_title);
		setDefaultPageImageDescriptor(JavaPluginImages.DESC_WIZBAN_NEWCLASS);
	}

	public void setAdapter(NewTypeWizardAdapter adapter) {
		this.adapter = adapter;
	}

	public NewClassWizard(NewTypeWizardAdapter adapter) {
		this();
		setAdapter(adapter);
	}

	public void addPages() {
		mainPage = new NewClassWizardPageEx();
		addPage(mainPage);
		if (adapter!=null) mainPage.init(adapter);
	}

    /**
     * @return
     */
    public String getClassName() {
        return this.mainPage.getTypeName();
    }
    
    public String getQualifiedClassName() {
    	String c = mainPage.getTypeName();
    	String p = mainPage.getPackageText();
    	if(p != null && p.length() > 0) c = p + "." + c; //$NON-NLS-1$
    	return c;
    }
   
	protected void finishPage(IProgressMonitor monitor) throws InterruptedException, CoreException {
	    mainPage.createType(monitor); // use the full progress monitor
	}
    
	protected boolean canRunForked() {
		return true;
	}

	protected ISchedulingRule getSchedulingRule() {
		return ResourcesPlugin.getWorkspace().getRoot(); // look all by default
	}
	
	public boolean performFinish() {
		IWorkspaceRunnable op= new IWorkspaceRunnable() {
			public void run(IProgressMonitor monitor) throws CoreException, OperationCanceledException {
				try {
					finishPage(monitor);
				} catch (InterruptedException e) {
					throw new OperationCanceledException(e.getMessage());
				}
			}
		};
		try {
			getContainer().run(canRunForked(), true, new WorkbenchRunnableAdapter(op, getSchedulingRule()));
		} catch (InvocationTargetException e) {
			ModelUIPlugin.getPluginLog().logError(e);
			return false;
		} catch  (InterruptedException e) {
			return false;
		}
		return true;
	}
}
