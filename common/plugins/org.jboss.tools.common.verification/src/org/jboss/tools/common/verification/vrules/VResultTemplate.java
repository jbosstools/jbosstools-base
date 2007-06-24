/*
 * VResultTemplate.java
 *
 * Created on July 28, 2003, 10:43 AM
 */

package org.jboss.tools.common.verification.vrules;

import java.beans.*;

/**
 *
 * @author  valera
 */
public class VResultTemplate {
    
    private String id;
    private String name;
    private String description;
    private String type;
    private int significance;
    private VMessageFormat format;
    private PropertyChangeSupport propertyChangeSupport;

    /** Creates a new instance of VResultTemplate */
    public VResultTemplate() {
        propertyChangeSupport = new PropertyChangeSupport(this);
    }
    
    /** Returns this template id.
     * @return Value of property id.
     *
     */
    public String getId() {
        return id;
    }    
    
    /** Sets this template id.
     * @param id New value of property id.
     *
     */
    public void setId(String id) {
        String old = this.id;
        this.id = id;
        propertyChangeSupport.firePropertyChange("id", old, id);
    }
    
    /** Returns this template name.
     * @return Value of property name.
     *
     */
    public String getName() {
        return name;
    }
    
    /** Sets this template name.
     * @param name New value of property name.
     *
     */
    public void setName(String name) {
        String old = this.name;
        this.name = name;
        propertyChangeSupport.firePropertyChange("name", old, name);
    }
    
    /** Returns this template description.
     * @return Value of property description.
     *
     */
    public String getDescription() {
        return description;
    }
    
    /** Sets this template description.
     * @param description New value of property description.
     *
     */
    public void setDescription(String description) {
        String old = this.description;
        this.description = description;
        propertyChangeSupport.firePropertyChange("description", old, description);
    }
    
    /** Returns this template type.
     * @return Value of property type.
     *
     */
    public String getType() {
        return type;
    }
    
    /** Sets this template type.
     * @param type New value of property type.
     *
     */
    public void setType(String type) {
        String old = this.type;
        this.type = type;
        propertyChangeSupport.firePropertyChange("type", old, type);
    }
    
    /** Returns this template significance.
     * @return Value of property significance.
     *
     */
    public int getSignificance() {
        return significance;
    }
    
    /** Sets this template significance.
     * @param significance New value of property significance.
     *
     */
    public void setSignificance(int significance) {
        int old = this.significance;
        if (significance > 10) {
            this.significance = 10;
        } else if (significance < 0) {
            this.significance = 0;
        } else {
            this.significance = significance;
        }
        propertyChangeSupport.firePropertyChange("significance", old, significance);
    }
    
    /** Returns this template format.
     * @return Value of property format.
     *
     */
    public VMessageFormat getFormat() {
        return format;
    }
    
    /** Sets this template format.
     * @param format New value of property format.
     *
     */
    public void setFormat(VMessageFormat format) {
        VMessageFormat old = this.format;
        this.format = format;
        propertyChangeSupport.firePropertyChange("format", old, format);
    }
    
    /** Creates new VResult object.
     */
    public VResult getResult(VObject sourceObject, Object sourcePosition,
            VObject targetObject, Object targetPosition, Object[] params) {
        VResult result = new VResult();
        result.setSignificance(this.significance);
        result.setSourceObject(sourceObject);
        result.setSourcePosition(sourcePosition);
        result.setTargetObject(targetObject);
        result.setTargetPosition(targetPosition);
        result.setType(this.type);
        if (format != null) {
            String message = format.format(params);
            VMessageFormat parent = format.getParent();
            if (parent != null) {
                Object[] params2 = new Object[] {sourceObject, sourcePosition,
                    targetObject, targetPosition, message, type, new Integer(significance)};
                message = parent.format(params2);
            }
            result.setMessage(message);
        }
        return result;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o instanceof VResultTemplate) {
            return this.id.equals(((VResultTemplate)o).id);
        }
        return false;
    }
    
    public int hashCode() {
        return id.hashCode();
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }

}
