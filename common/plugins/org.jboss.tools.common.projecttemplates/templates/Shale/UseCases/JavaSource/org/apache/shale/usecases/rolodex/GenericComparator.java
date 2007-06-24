/*
 * Copyright 2004-2005 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shale.usecases.rolodex;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * <p>Generic comparator that uses a list of property names to compare
 * the state of two objects using the reflection API.  The property names 
 * are passed via a comma delimited.  The collating sequence is determined 
 * by the <code>sortAcending</code> attribute.</p>
 */
public class GenericComparator implements Comparator {
    
    /**
     * <p>Common logger utility.</p>
     */
    public static Log log;
    static {
        log = LogFactory.getLog(GenericComparator.class);
    }   

    /**
     * <p>Determines the collating sequence.  A <code>true</code> value
     * will sort acending; otherwise, the list will be sorted in descending
     * order.</p>
     */
    private boolean sortAscending = true;
   
    /**
     * <p>Holds an array of property names in the target object that
     * will be use to compare the two objects.
     * </p>
     */
    protected List sortBy;
    
    /**
     * <p>Returns <code>true</code> if the collection should be sorted ascending by
     * the <code>sortBy</code> properties list.<p>
     */
    public boolean getSortAscending() {
        return sortAscending;
    }

    /**
     * <p>Sets the sequence the collection should be sorted.  A <code>true</code>
     * will result in sort ascending.</p>
     */
    public void setSortAscending(boolean value) {
        sortAscending = value;
    }
   
    /**
     * <p>Passed a comma delimited list of property names to compare two object by.<p>
     */
    public void setSortBy(String properties) {
       StringTokenizer tokenizer = new StringTokenizer(properties, ",");
       sortBy = new ArrayList();
       while (tokenizer.hasMoreTokens()) {
          String token = tokenizer.nextToken().trim();
          sortBy.add(token);
       }
       
    }
    
    /**
     * <p>Returns a comma delimited list of property names used to compare 
     * two objects.</p>
     */
    public String getSortBy() {
        StringBuffer tmp = new StringBuffer();
        if (sortBy != null) {
           Iterator li = sortBy.iterator();
           while (li.hasNext()) {
              if (tmp.length() > 0) {
                 tmp.append(", ");
              }
              tmp.append(li.next());
           }
        }
        
        return tmp.toString();
    }
    
    /**
     * <p>Compares the property names in the <code>sortBy</code> list with
     * the target sortable objects.  The collating sequence is determined 
     * by the <code>sortAcending</code> attribute.
     * </p>
     * 
     * @param o1 -
     *            target object 1
     * @param o2 -
     *            target object 2
     * @return - integer value representing the comparison of the two objects
     *         key properties.
     */
    public int compare(Object o1, Object o2) {
        Iterator it = sortBy.iterator();
        int result = 0;
        while (result == 0 && it.hasNext()) {
            String nextProperty = (String) it.next();

            Object col1 = null;
            Object col2 = null;

            try {
                col1 = BeanUtils.getProperty(o1, nextProperty);
            } catch (Exception e) {
                log.error("Error finding property " + nextProperty
                        + "to sort on in the target object", e);
            }

            try {
                col2 = BeanUtils.getProperty(o2, nextProperty);
            } catch (Exception e) {
                log.error("Error finding property " + nextProperty
                        + "to sort on in the target object", e);
            }

            if ((col1 == null) && (col2 == null))
                result = 0;
            else if ((col1 == null) && (col2 != null))
                result = -1;
            else if ((col2 == null) && (col1 != null))
                result = 1;
            else if ((col1 instanceof Comparable)
                    && (col2 instanceof Comparable))
                result = ((Comparable) col1).compareTo(col2);
            else {

                result = ((Comparable) col1.toString()).compareTo(col2
                        .toString());
            }

            col1 = null;
            col2 = null;

        }
        return  (sortAscending ? 1 : -1) * result;
    }

}
