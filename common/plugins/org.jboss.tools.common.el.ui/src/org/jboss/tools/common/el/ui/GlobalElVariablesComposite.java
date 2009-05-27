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

import org.jboss.tools.common.el.core.GlobalELReferenceList;
import org.jboss.tools.common.resref.core.ResourceReference;
import org.jboss.tools.common.resref.core.ResourceReferenceList;
import org.jboss.tools.common.resref.ui.AbstractResourceReferencesComposite;
import org.jboss.tools.common.resref.ui.BaseAddReferenceSupport;
import org.jboss.tools.common.resref.ui.ResourceReferencesTableProvider;


/**
 * Composite class for the global el variables.
 * @author Evgenij Stherbin
 *
 */
public class GlobalElVariablesComposite extends AbstractResourceReferencesComposite {

    /**
     * @see org.jboss.tools.common.resref.core.AbstractResourceReferencesComposite#createGroupLabel()
     */
    @Override
    protected String createGroupLabel() {
        return ""; //$NON-NLS-1$
    }

    /**
     * @see org.jboss.tools.common.resref.core.AbstractResourceReferencesComposite#createTableProvider(java.util.List)
     */
    @Override
    protected ResourceReferencesTableProvider createTableProvider(List dataList) {
        return ResourceReferencesTableProvider.getGlobalELTableProvider(dataList);
    }

    /**
     * @see org.jboss.tools.common.resref.core.AbstractResourceReferencesComposite#getEntity()
     */
    @Override
    protected String getEntity() {
        return "VPEGlobalElReference"; //$NON-NLS-1$
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

	@Override
	protected void add(int index) {
		ResourceReference defaultRef = getDefaultResourceReference();
		
		boolean ok = BaseAddReferenceSupport.add(file, defaultRef, getReferenceArray(), getEntity());
		if(!ok) return;
		dataList.add(defaultRef);
		update();
		table.setSelection(dataList.size() - 1);
	}

	@Override
	protected void edit(int index) {
		if(index < 0) return;
		ResourceReference defaultRef = getReferenceArray()[index];
		boolean ok = BaseAddReferenceSupport.edit(file, defaultRef, getReferenceArray(), getEntity());
		if(!ok) return;
		update();
	}

}
