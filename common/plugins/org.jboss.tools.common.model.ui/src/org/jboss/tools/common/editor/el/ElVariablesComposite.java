package org.jboss.tools.common.editor.el;

import java.util.List;

import org.jboss.tools.common.editor.messages.Messages;
import org.jboss.tools.common.editor.rreferences.ResourceReferenceList;
import org.jboss.tools.common.editor.rreferences.ResourceReferencesComposite;
import org.jboss.tools.common.editor.rreferences.ResourceReferencesTableProvider;

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
