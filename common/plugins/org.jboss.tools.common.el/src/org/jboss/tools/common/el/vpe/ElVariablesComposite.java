package org.jboss.tools.common.el.vpe;

import java.util.List;

import org.jboss.tools.common.rreferences.core.ResourceReferenceList;
import org.jboss.tools.common.rreferences.core.ResourceReferencesComposite;
import org.jboss.tools.common.rreferences.core.ResourceReferencesTableProvider;
import org.jboss.tools.common.rreferences.messages.Messages;

/**
 * The Class ElVariablesComposite.
 */
public class ElVariablesComposite extends ResourceReferencesComposite {

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
     * @see ResourceReferencesComposite#createGroupLabel()
     */
    @Override
    protected String createGroupLabel() {
        return Messages.SUBSTITUTED_EL_EXPRESSIONS;
    }



}
