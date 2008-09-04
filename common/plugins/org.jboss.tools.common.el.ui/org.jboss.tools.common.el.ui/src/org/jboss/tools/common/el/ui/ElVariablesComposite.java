package org.jboss.tools.common.el.ui;

import java.util.List;

import org.jboss.tools.common.el.vpe.ELReferenceList;
import org.jboss.tools.common.resref.core.ResourceReferenceList;
import org.jboss.tools.common.resref.ui.AbstractResourceReferencesComposite;
import org.jboss.tools.common.resref.ui.ResourceReferencesTableProvider;

/**
 * The Class ElVariablesComposite.
 */
public class ElVariablesComposite extends AbstractResourceReferencesComposite {

    /**
     * Creates the table provider.
     * 
     * @param dataList the data list
     * g
     * @return the resource references table provider
     */
    @Override
    protected ResourceReferencesTableProvider createTableProvider(List dataList) {
        return ResourceReferencesTableProvider.getELTableProvider(dataList);
    };


    /**
     * Gets the entity.
     * 
     * @return the entity
     */
    @Override
    protected String getEntity() {
        return (file != null) ? "VPEElReference" : "VPEElReferenceExt";
    }

    /**c
     * Gets the reference list.
     * 
     * @return the reference list
     */
    @Override
    protected ResourceReferenceList getReferenceList() {
        return ELReferenceList.getInstance();
    }

    /**
     * @see AbstractResourceReferencesComposite#createGroupLabel()
     */
    @Override
    protected String createGroupLabel() {
        return Messages.SUBSTITUTED_EL_EXPRESSIONS;
    }


	@Override
	protected void add(int index) {
		// TODO Auto-generated method stub
		
	}


	@Override
	protected void edit(int index) {
		// TODO Auto-generated method stub
		
	}



}
