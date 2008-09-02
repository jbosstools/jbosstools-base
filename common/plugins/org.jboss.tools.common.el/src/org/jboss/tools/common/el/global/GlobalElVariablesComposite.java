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

package org.jboss.tools.common.el.global;

import java.util.List;

import org.jboss.tools.common.resref.core.ResourceReference;
import org.jboss.tools.common.resref.core.ResourceReferenceList;
import org.jboss.tools.common.resref.core.ResourceReferencesComposite;
import org.jboss.tools.common.resref.core.ResourceReferencesTableProvider;


/**
 * Composite class for the global el variables.
 * @author Evgenij Stherbin
 *
 */
public class GlobalElVariablesComposite extends ResourceReferencesComposite {

    /**
     * @see org.jboss.tools.common.resref.core.ResourceReferencesComposite#createGroupLabel()
     */
    @Override
    protected String createGroupLabel() {
        return "";
    }

    /**
     * @see org.jboss.tools.common.resref.core.ResourceReferencesComposite#createTableProvider(java.util.List)
     */
    @Override
    protected ResourceReferencesTableProvider createTableProvider(List dataList) {
        return ResourceReferencesTableProvider.getGlobalELTableProvider(dataList);
    }

    /**
     * @see org.jboss.tools.common.resref.core.ResourceReferencesComposite#getEntity()
     */
    @Override
    protected String getEntity() {
        return (file != null) ? "VPEGlobalElReference" : "VPEGlobalElReferenceExt";
    }

    /**
     * @see org.jboss.tools.common.resref.core.ResourceReferencesComposite#getReferenceList()
     */
    @Override
    protected ResourceReferenceList getReferenceList() {
        return GlobalELReferenceList.getInstance();
    }
    
    @Override
    protected ResourceReference getDefaultResourceReference() {
        ResourceReference rf = new ResourceReference("", ResourceReference.GLOBAL_SCOPE);
        rf.setGlobal(true);
        return rf;
    }

}
