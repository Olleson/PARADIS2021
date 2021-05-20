// Peter Idestam-Almquist, 2017-01-20.

package Java.annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

@Target(ElementType.FIELD)
public @interface GuardedBy { String value() default ""; }