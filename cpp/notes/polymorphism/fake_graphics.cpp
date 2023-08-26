/* polymorphism/fake_graphics.cpp */
#include <stdio.h>

// Whenever a class gets virtual functions, every object of this class
// (i. e. of this class or any of it's children)
// gets an implicit member vmtp, a pointer to a virtual function table
// (static for every class in the hierarchy). The correct function is
// called by reading vmtp, looking up the function address in the
// vtable, and calling it. This is done for every call.
// vmtp initialization code is automatically inserted at the beginning
// of the constructor

class Pixel {
protected:
    float x, y;
    int color;

public:
    Pixel(float ax, float ay, int acolor)
        : x(ax), y(ay), color(acolor) {}

    // It is said that if a class has virt funcs => we want to use polymorphism,
    // we should always make the destructor virtual for the following reason:
    // if we do Parent *p = new Child(...), and then do delete p, if the 
    // destructor is not virtual, a destructor for Parent will be called,
    // failing to call all other destructors in the heirarchy. And if
    // it is virtual, all of them will get called in reverse order
    // (through ~Child(), kept in the vtable).
    // It might not be always necessary (like here), but if we rely on 
    // virt funcs anyway, one more vtable entry is not that bad
    virtual ~Pixel() {}

    // virtual func means that when called, it may be implemented differently
    // in a child class, thus we must determine, what func to call
    virtual void Show();
    virtual void Hide();

    void Move(float nx, float ny);
};

void Pixel::Show()
{
    printf("Show pixel at (%.2f, %.2f) with color %X\n", x, y, color);
}

void Pixel::Hide()
{
    printf("Hide pixel at (%.2f, %.2f) with color %X\n", x, y, color);
}

void Pixel::Move(float nx, float ny)
{
    Hide();
    x = nx;
    y = ny;
    Show();
}

class Circle : public Pixel {
    float radius;

public:
    Circle(float ax, float ay, float aradius, int acolor)
        : Pixel(ax, ay, acolor), radius(aradius) {}

    // If we wanted to implement our own Show-Hide simply here
    // without any virtual funcs, but
    // leave Move to Pixel, it wouldn't work correctly, since Pixel::Move
    // calls Pixel::Show and Pixel::Hide, not Circle:: ones

    // But, since parent funcs are virtual, their calls in Pixel::Move
    // deduce, that the object is a circle, and call these funcs for
    // Circle:: . We do not have to write virtual here, cause these
    // ones become virtual automatically, but let's do it for readability
    virtual void Show();
    virtual void Hide();
};

void Circle::Show()
{
    printf("Show circle at (%.2f, %.2f) with raduis %f and color %X\n", 
           x, y, radius, color);
}

void Circle::Hide()
{
    printf("Hide circle at (%.2f, %.2f) with raduis %f and color %X\n", 
           x, y, radius, color);
}

int main()
{
    Circle c(4, 5, 1, 0xFF00FF);

    // This is not done through a vtable: the compiler already knows, what
    // functions to use, and just inserts their code
    c.Show();
    c.Hide();
    putchar('\n');

    // Here, the vtable will be used, since pointers can be implicitly cast
    // to parent class pointers, thus the compiler does not know, what
    // the pointer is. (for example, you pass a game object pointer to a func,
    // and you want to call the right Update for this type of game object)
    // Same goes for references, obviously
    Circle *p = new Circle(0.1, 0.05, 1.2, 0);
    p->Show();
    p->Hide();
    putchar('\n');

    // Same with in-class methods that use virtual methods:
    // since they use "this", which is a pointer
    c.Move(0.3, 1.6);

    return 0;
}
