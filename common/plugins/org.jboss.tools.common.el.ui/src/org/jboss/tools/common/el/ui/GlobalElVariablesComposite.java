/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.el.ui;

import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.el.core.GlobalELReferenceList;
import org.jboss.tools.common.resref.core.ResourceReference;
import org.jboss.tools.common.resref.core.ResourceReferenceList;
import org.jboss.tools.common.resref.ui.ResourceReferencesTableProvider;
import org.jboss.tools.vpe.resref.core.AbstractResourceReferencesComposite;
import org.jboss.tools.vpe.resref.core.GlobalELReferenceWizardDialog;
import org.jboss.tools.vpe.resref.core.ReferenceWizardDialog;


/**
 * Composite class for the global el variables.
 * @author Evgenij Stherbin
 *
 */
public class GlobalElVariablesComposite extends AbstractResourceReferencesComposite {

    /**
     * @see org.jboss.tools.common.resref.core.AbstractResourceReferencesComposite#createTableProvider(java.util.List)
     */
    @Override
    protected ResourceReferencesTableProvider createTableProvider(List dataList) {
        return ResourceReferencesTableProvider.getGlobalELTableProvider(dataList);
    }

    /**
     * @see org.jboss.tools.common.resref.core.AbstractResourceReferencesComposite#getReferenceList()
     */
    @Override
    protected ResourceReferenceList getReferenceList() {
        return GlobalELReferenceList.getInstance();
    }
    
    @Override
    protected ResourceReference getDefaultResourceReference() {
        ResourceReference rf = new ResourceReference("", ResourceReference.GLOBAL_SCOPE); //$NON-NLS-1$
        rf.setGlobal(true);
        return rf;
    }
    
    protected ReferenceWizardDialog getDialog(ResourceReference resref) {
        return new GlobalELReferenceWizardDialog(
				PlatformUI.getWorkbench().getDisplay().getActiveShell(), fileLocation, resref, getReferenceArray());
    }

	@Override
	protected void add(int index) {
		ResourceReference resref = getDefaultResourceReference();
		int returnCode = -1;
		ReferenceWizardDialog  d = getDialog(resref);
		if (null != d) {
			returnCode = d.open();
		}
		if (Dialog.OK == returnCode) {
			dataList.add(resref);
			update();
			table.setSelection(dataList.size() - 1);
		}
	}

	@Override
	protected void edit(int index) {
		if(index < 0) {
			return;
		}
		ResourceReference resref = getReferenceArray()[index];
		int returnCode = -1;
		ReferenceWizardDialog  d = getDialog(resref);
		if (null != d) {
			returnCode = d.open();
		}
		if (Dialog.OK == returnCode) {
			update();
		}
	}

}
