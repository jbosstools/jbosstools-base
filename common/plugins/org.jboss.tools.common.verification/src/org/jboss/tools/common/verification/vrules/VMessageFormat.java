/*
 * VMessageFormat.java
 *
 * Created on July 28, 2003, 10:44 AM
 */

package org.jboss.tools.common.verification.vrules;

import java.text.*;
import java.util.Locale;

/**
 *
 * @author  valera
 */
public class VMessageFormat {
    
    private String id;
    private String pattern;
    private Locale locale;
    private VMessageFormat parent;
    private MessageFormat format;
    
    /** Creates a new instance of VMessageFormat */
    public VMessageFormat() {
    }
    
    public VMessageFormat(String pattern) {
        setPattern(pattern);
    }
    
    /** Returns this format id.
     * @return Value of property id.
     */
    public String getId() {
        return id;
    }
    
    /** Sets this format id.
     * @param id New value of property id.
     *
     */
    public void setId(String id) {
        this.id = id;
    }
    
    /** Returns this format locale
     */
    public Locale getLocale() {
        return locale;
    }
    
    /** Sets this format locale
     */
    public void setLocale(Locale locale) {
        this.locale = locale;
        if (format != null) {
            format.setLocale(locale);
        }
    }
    
    /** Returns this format pattern.
     * @return Value of property pattern.
     *
     */
    public String getPattern() {
        return pattern;
    }
    
    /** Sets this format pattern.
     * @param pattern New value of property pattern.
     *
     */
    public void setPattern(String pattern) {
        if (format != null) {
            format.applyPattern(pattern);
        }
        this.pattern = pattern;
    }
    
    /** Returns parent format.
     * @return Value of property parent.
     *
     */
    public VMessageFormat getParent() {
        return parent;
    }
    
    /** Sets parent format.
     * @param parent New value of property parent.
     *
     */
    public void setParent(VMessageFormat parent) {
        this.parent = parent;
    }
    
    /** Formats an array of objects to produce a string.
     */
    public String format(Object[] objects) {
        if (format == null) {
            format = new MessageFormat(pattern);
            if (locale != null) format.setLocale(locale);
        }
        return format.format(objects);
    }
    
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o instanceof VMessageFormat) {
            return this.id.equals(((VMessageFormat)o).id);
        }
        return false;
    }
    
    public int hashCode() {
        return id.hashCode();
    }

}
