/*******************************************************************************
 * Copyright (c) 2007-2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.ui.bot.ext;

import java.util.Iterator;
import java.util.List;

/**
 * Utils for Formating
 * @author Vladimir Pakan
 */
public class FormatUtils {
  /**
   * Get formatted difference between list0 and list1
   * @param list0
   * @param list1
   * @return
   */
  public static String getListsDiffFormatted (List<String> list0 , List<String> list1){
    StringBuffer sb = new StringBuffer("");
    if (list0 == null && list1 == null){
      sb.append("<null> == <null>");
    }else if (list0 == null){
      sb.append("<null> != <not null>");
    }else if (list1 == null){
      sb.append("<null> != <not null>");
    } else{
      Iterator<String> iterator0 = list0.iterator();
      Iterator<String> iterator1 = list1.iterator();
      boolean continueIteration = iterator0.hasNext() || iterator1.hasNext();
      while (continueIteration){
        String item0 = "<null>";
        String item1 = "<null>";
        sb.append("  ");
        if (iterator0.hasNext()){
          item0 = iterator0.next();
        }
        if (iterator1.hasNext()){
          item1 = iterator1.next();
        }
        sb.append("  ");
        sb.append(item0);
        sb.append(item0.equals(item1) ? " == " : " != ");
        sb.append(item1);
        continueIteration = iterator0.hasNext() || iterator1.hasNext();
        if (continueIteration){
          sb.append("\n");
        }
      }
    }
    System.out.println(sb.toString());
    for (String item : list1){
      System.out.println("result.add(\"" + item.replaceAll("\"", "\\\"") + "\");");
    }

    return sb.toString();
  }


}
