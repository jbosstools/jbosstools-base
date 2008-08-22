

package org.jboss.tools.common.editor.el;


import org.eclipse.core.runtime.QualifiedName;
import org.jboss.tools.common.editor.rreferences.ResourceReferenceList;


/**
 * The Class ELReferenceList.
 */
public class ELReferenceList extends ResourceReferenceList {
    
    /** The PROPERT y_ NAME. */
    private static QualifiedName PROPERTY_NAME = new QualifiedName("", "org.jboss.tools.vpe.editor.css.ELReference");

    /** The instance. */
    static ELReferenceList instance = new ELReferenceList();

    /**
     * Gets the instance.
     * 
     * @return the instance
     */
    public static ELReferenceList getInstance() {
        return instance;
    }

    /**
     * Gets the property name.
     * 
     * @return the property name
     */
    protected QualifiedName getPropertyName() {
        return PROPERTY_NAME;
    }
}
